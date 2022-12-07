import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static com.google.common.base.Splitter.fixedLength;
import static com.google.common.collect.Lists.newArrayList;
import static com.google.common.collect.Lists.reverse;
import static java.lang.String.valueOf;
import static java.lang.System.getenv;
import static java.util.regex.Pattern.matches;
import static java.util.stream.Collectors.joining;
import static java.util.stream.IntStream.range;
import static java.util.stream.IntStream.rangeClosed;

public class App {
    String solvePart1(final Map<Integer, ArrayDeque<Character>> stacks, final List<Move> moves) {
        moves.forEach(move -> rangeClosed(1, move.amount).forEach(ignore -> stacks.get(move.to).push(stacks.get(move.from).pop())));
        return stacks.values().stream().map(stack -> valueOf(stack.peek())).collect(joining());
    }

    String solvePart2(final Map<Integer, ArrayDeque<Character>> stacks, final List<Move> moves) {
        moves.forEach(move -> reverse(rangeClosed(1, move.amount).mapToObj(ignore -> stacks.get(move.from).pop()).toList()).forEach(crate -> stacks.get(move.to).push(crate)));
        return stacks.values().stream().map(stack -> valueOf(stack.peek())).collect(joining());
    }

    public static void main(final String[] args) throws IOException {
        final Path input = Path.of("input.txt");
        final Map<Integer, ArrayDeque<Character>> stacks = new HashMap<>();

        Files.lines(input).takeWhile(line -> !matches("[1-9 ]+", line))
                .map(line -> newArrayList(fixedLength(4).split(line + " "))).forEachOrdered(row -> {
                    range(0, row.size()).filter(index -> !row.get(index).isBlank()).forEach(index -> {
                        var stack = stacks.getOrDefault(index + 1, new ArrayDeque<>());
                        stack.addLast(row.get(index).charAt(1));
                        stacks.put(index + 1, stack);
                    });
                });

        final List<Move> moves = Files.lines(input).dropWhile(line -> !line.isBlank()).skip(1).map(line ->
                Move.fromList(Stream.of(line.replaceAll("\\D+", " ").trim().split(" ")).map(Integer::parseInt).toList())).toList();

        System.out.println("part2".equalsIgnoreCase(getenv("part")) ? new App().solvePart2(stacks, moves) : new App().solvePart1(stacks, moves));
    }

    record Move (int amount, int from, int to) {
        static Move fromList(final List<Integer> parsedInts) {
            return new Move(parsedInts.get(0), parsedInts.get(1), parsedInts.get(2));
        }
    }
}
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import static java.lang.System.getenv;

public class App {
    record Game (Shape opponent, Shape you) {
        long outcome() {
            return you.score + (you.defeats(opponent) ? 6 : opponent.defeats(you) ? 0 : 3);
        }
        static Game fromShapes(final String[] shapes) {
            return new Game(Shape.fromLetter(shapes[0]), Shape.fromLetter(shapes[1]));
        }
        static Game fromStrategy(final String[] shapes) {
            final Shape opponent = Shape.fromLetter(shapes[0]);
            return new Game(opponent, Shape.fromStrategy(shapes[1], opponent));
        }
    }

    long solvePart1(final Stream<String> lines) {
        return lines.map(line -> Game.fromShapes(line.split(" "))).mapToLong(Game::outcome).sum();
    }

    long solvePart2(final Stream<String> lines) {
        return lines.map(line -> Game.fromStrategy(line.split(" "))).mapToLong(Game::outcome).sum();
    }

    public static void main(final String[] args) throws IOException {
        final Stream<String> lines = Files.lines(Path.of("input.txt"));
        System.out.println("part2".equalsIgnoreCase(getenv("part")) ? new App().solvePart2(lines) : new App().solvePart1(lines));
    }

    private enum Shape {
        ROCK(1), PAPER(2), SCISSORS(3);
        static {
            SCISSORS.defeats = PAPER; PAPER.defeats = ROCK; ROCK.defeats = SCISSORS;
            SCISSORS.losesAgainst = ROCK; PAPER.losesAgainst = SCISSORS; ROCK.losesAgainst = PAPER;
        }

        private Shape defeats, losesAgainst;
        private final int score;

        Shape(int score) {
            this.score = score;
        }

        public boolean defeats(final Shape shape) {
            return defeats == shape;
        }

        static Shape fromStrategy(final String strategy, final Shape opponent) {
            return switch (strategy) {
                case "X" -> opponent.defeats;
                case "Y" -> opponent;
                case "Z" -> opponent.losesAgainst;
                default -> throw new IllegalStateException();
            };
        }

        static Shape fromLetter(final String letter) {
            return switch (letter) {
                case "A", "X" -> ROCK;
                case "B", "Y" -> PAPER;
                case "C", "Z" -> SCISSORS;
                default -> throw new IllegalStateException();
            };
        }
    }
}
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static java.lang.Character.getNumericValue;
import static java.lang.System.getenv;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;

public class App {
    long solvePart1(final TreeGrid treeGrid) {
        return Set.copyOf(concat(treeGrid.allVisbleInColumns().stream(), treeGrid.allVisbleInRows().stream()).toList()).size();
    }

    long solvePart2(final TreeGrid treeGrid) {
        return Set.copyOf(concat(treeGrid.columns.values().stream().flatMap(Collection::stream), treeGrid.columns.values().stream().flatMap(Collection::stream)).toList())
                .stream().mapToLong(treeGrid::scenicScoreFor).max().getAsLong();
    }

    public static void main(final String[] args) throws IOException {
        final AtomicInteger y = new AtomicInteger(0);
        final TreeGrid treeGrid = new TreeGrid();
        Files.lines(Path.of("input.txt")).forEach(line -> {
            range(0, line.length()).forEach(x -> {
                treeGrid.add(x, y.get(), getNumericValue(line.charAt(x)));
            });
            y.incrementAndGet();
        });

        System.out.println("part2".equalsIgnoreCase(getenv("part")) ? new App().solvePart2(treeGrid) : new App().solvePart1(treeGrid));
    }

    record Tree (int height, int x, int y) {
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Tree tree = (Tree) o;
            return x == tree.x && y == tree.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }
    static class TreeGrid {
        final Map<Integer, List<Tree>> columns = new HashMap<>();
        final Map<Integer, List<Tree>> rows = new HashMap<>();

        long scenicScoreFor(final Tree tree) {
            if(tree.x == 0 || tree.x == columns.size() - 1
                    || tree.y == 0 || tree.y == rows.size() - 1)
                return 0;
            final List<Tree> column = columns.get(tree.x);
            final List<Tree> row = this.rows.get(tree.y);
            long up = 0, down = 0, left = 0, right = 0;

            for(int index = tree.y - 1; index >= 0; index--) {
                up++;
                if(column.get(index).height >= tree.height)
                    break;
            }
            for(int index = tree.y + 1; index < column.size(); index++) {
                down++;
                if(column.get(index).height >= tree.height)
                    break;
            }
            for(int index = tree.x - 1; index >= 0; index--) {
                left++;
                if(row.get(index).height >= tree.height)
                    break;
            }
            for(int index = tree.x + 1; index < row.size(); index++) {
                right++;
                if(row.get(index).height >= tree.height)
                    break;
            }
            //System.out.println("up * down * left * right = " + up + " * " + down + " * " + left + " * " + right);
            return up * down * left * right;
        }

        void add(final int x, final int y, final int treeHeight) {
            final Tree tree = new Tree(treeHeight, x, y);
            final List<Tree> column = getOrCreate(columns, x);
            final List<Tree> row = getOrCreate(rows, y);
            column.add(y, tree);
            row.add(x, tree);
        }

        List<Tree> getOrCreate(final Map<Integer, List<Tree>> directions, final int index) {
            final List<Tree> direction = directions.getOrDefault(index, new ArrayList<>());
            directions.put(index, direction);
            return direction;
        }

        List<Tree> allVisbleInColumns() {
            return getAllVisibleIn(columns);
        }

        List<Tree> allVisbleInRows() {
            return getAllVisibleIn(rows);
        }

        private List<Tree> getAllVisibleIn(final Map<Integer, List<Tree>> directions) {
            final List<Tree> visible = new ArrayList<>();
            directions.forEach((x, direction) -> {
                if (x == 0 || x == directions.size() - 1) {
                    visible.addAll(direction);
                } else {
                    visible.addAll(range(1, direction.size()).filter(index -> {
                        final Tree tree = direction.get(index);
                        return range(0, index).allMatch(above -> direction.get(above).height < tree.height)
                                || range(index + 1, direction.size()).allMatch(above -> direction.get(above).height < tree.height);
                    }).mapToObj(direction::get).toList());
                }
            });
            return visible;
        }

        @Override
        public String toString() {
            return "TreeGrid{" +
                    "columns=" + columns +
                    ", rows=" + rows +
                    '}';
        }
    }
}
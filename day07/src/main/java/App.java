import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

import static java.lang.System.getenv;

public class App {
    long solvePart1(final Filesystem filesystem) {
        return filesystem.allDirectories.stream().filter(directory -> directory.size() <= 100000).mapToLong(Directory::size).sum();
    }

    long solvePart2(final Filesystem filesystem) {
        final long totalDiskSpace = 70000000;
        final long requiredDiskSpace = 30000000;
        final long unusedDiskSpace = totalDiskSpace - filesystem.root.size();
        return filesystem.allDirectories.stream().filter(directory -> directory.size() + unusedDiskSpace >= requiredDiskSpace)
                .mapToLong(Directory::size).min().orElseThrow();
    }

    public static void main(final String[] args) throws IOException {
        final Filesystem filesystem = new Filesystem(new Directory("root"));
        try(Stream<String> lines = Files.lines(Path.of("input.txt"))) {
            lines.skip(1).forEach(line -> {
                if(line.startsWith("$")) {
                    if(line.startsWith("$ cd ")) {
                        final String dir = line.substring(5);
                        if (dir.equals("..")) {
                            filesystem.setCurrentDirectory(filesystem.currentDirectory().parent());
                        } else {
                            final Directory newDirectory = new Directory(dir);
                            filesystem.allDirectories.add(newDirectory);
                            filesystem.currentDirectory().addFile(newDirectory);
                            filesystem.setCurrentDirectory(newDirectory);
                        }
                    }
                } else if(!line.startsWith("dir")) {
                    final String[] split = line.split(" ");
                    filesystem.currentDirectory().addFile(new File(split[1], Long.parseLong(split[0])));
                }
            });
        }

        System.out.println("part2".equalsIgnoreCase(getenv("part")) ? new App().solvePart2(filesystem) : new App().solvePart1(filesystem));
    }



    static class Filesystem {
        final List<Directory> allDirectories = new ArrayList<>();
        final Directory root;
        Directory currentDirectory;

        Filesystem(Directory root) {
            this.root = root;
            this.currentDirectory = root;
            this.allDirectories.add(root);
        }

        Directory currentDirectory() {
            return currentDirectory;
        }

        public void setCurrentDirectory(final Directory currentDirectory) {
            this.currentDirectory = currentDirectory;
        }
    }

    static class File {
        private long size;
        private final String name;

        File(final String name) {
            this.name = name;
        }

        File(final String name, final long size) {
            this.name = name;
            this.size = size;
        }

        public long size() {
            return size;
        }

        public String name() {
            return name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            File file = (File) o;
            return name.equals(file.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }
    }

    static class Directory extends File {
        private Directory parent;
        private final Set<File> files = new HashSet<>();

        Directory(final String name) {
            super(name);
        }

        public long size() {
            return files.stream().mapToLong(File::size).sum();
        }

        public void addFile(final File file) {
            this.files.add(file);
        }

        public void addFile(final Directory directory) {
            this.files.add(directory);
            directory.setParent(this);
        }

        Directory parent() {
            return parent;
        }

        public void setParent(final Directory directory) {
            this.parent = directory;
        }
    }
}
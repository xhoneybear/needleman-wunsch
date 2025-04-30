# Needleman-Wunsch Algorithm

Scala implementation of the Needleman-Wunsch algorithm.

Compares two strings and displays their similarity statistics.

### Why Scala?

Why not?

Now, I know Python and R are the most popular languages for bioinformatics, but I always like a little challenge. It's a small project for the bioinformatics class I'm taking and it would just be way too easy for me to do it in Python. I like Scala and I wanted to get a bit more grip of it for a while, so... here we are.

## Functionalities
- [x] Parse strings as arguments or from a file
- [x] Display similarity statistics
- [x] Display similarity matrix
- [x] Control penalties for match, gap and mismatch
- [ ] Linear or exponential penalties
- [ ] Config files for steady setups

## How to run

Use sbt. Run `sbt "run -h"`. The help message will show up and all options will be listed.

### Note

ZDepth layout used for the similarity matrix is not present in nspl releases yet. It is packaged inside this project in a separate Scala file. If you prefer, you can add the dependency manually from [nspl's main branch](https://github.com/pityka/nspl).

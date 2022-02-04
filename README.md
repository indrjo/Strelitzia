
# Strelitzia

Actually, this is a *strelitzia* (credit [wikipedia](https://commons.wikimedia.org/wiki/File:Strelitzia_larger.jpg#/media/File:Strelitzia_larger.jpg)):

![Strelitzia](https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Strelitzia_larger.jpg/1200px-Strelitzia_larger.jpg)

But it is also the name of this little Haskell program.

*Probably, you don't need such program since it is thought for a very personal and particular task.*


## (Un)Install

Throughout this section, it is assumed that you've previously ```cd```-ed into the directory where ```strelitzia.hs``` lies.

To compile the program, just

```
$ ./install.sh
```

If you want to get rid of the executable,

```
$ ./remove.sh
```

is enough.


## Usage

```
$ strelitzia --use THIS-FILE
```

where ```THIS-FILE``` is the file the user provides for parsing. You have to give a file to read, otherwise the program will do nothing.


## Options

```
$ strelitzia -h
[...]
Application Options:
  --do-nothing :: bool
    The program will only show what is going to do.
    default: false
  --quiet :: bool
    stdout messages are silenced.
    default: false
  --no-errs :: bool
    stderr messages are silenced.
    default: false
  --fatal :: bool
    Make warnings fatal.
    default: false
  --use :: maybe<text>
    Indicate to strelitzia what file to read.
```

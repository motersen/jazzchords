# jazzchords.scm

Jazz chord changes notation for lilypond

![example](https://user-images.githubusercontent.com/6299210/109810436-d0971e00-7c29-11eb-8046-43387f2b080d.png)

## Commands
- `\jazzchord <chordsymbol>` *markup command*: generate chord markup
- `\jazz <chordsymbol>` *scheme function*: display chord as a mark at beginning of current measure
- `\jc <chordsymbol> <music>` *scheme function*: display chord attached to music

## How to use

To use the commands in your project, clone this repository or download
[jazzchords.scm](https://raw.githubusercontent.com/motersen/jazzchords/master/jazzchords.scm)
and tell guile to load it by including
```lilypond
$(load "path/to/jazzchords.scm")
```
in your score. Have a look at [example.ly](https://github.com/motersen/jazzchords/blob/master/example/example.ly) for a complete example.

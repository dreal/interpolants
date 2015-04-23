
## Compiling

This project requires java 7.
You can build it using [sbt](http://www.scala-sbt.org/).
To install sbt follow the instructions at [http://www.scala-sbt.org/release/tutorial/Setup.html](http://www.scala-sbt.org/release/tutorial/Setup.html).

Then, in a console, execute:
```
$ sbt
> compile
```


## Running

```
$ sbt
> run -a "(and (= x (sin t)) (= y (cos t)))" -b "(> (+ (^ x 2) (^ y 2)) 1.2)" -d 0.01 -lb 0
```

currently fails because not everything is implemented

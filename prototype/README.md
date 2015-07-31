
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

```
$ sbt
> run -fa file1 -fb file2 -d 0.01 -lb -2 -ub 2
```
where `file1` and `file2` contains the A and B part of the formula

`-lb` and `-ub` specify the lower and upper bounds for the variables.
The default bounds are `-10` and `10`.

`-d` specify δ (default: `0.1`).


## ToDo

* boolean structure: extended Pudlak from [Yorsh & Musuvati 05](http://link.springer.com/chapter/10.1007%2F11532231_26)

     we have a resoltution proof with clause in A, B, and C where C is a set of conflict clauses.
     For each c ∈ C, ¬c is a conjunction that is unsat in QF_NRA.

     Let PI be a partial interpolant:

     - for the leaves of the proof
         * c ∈ A, PI(c) = false
         * c ∈ B, PI(c) = true
         * c ∈ C, PI(c) = I(¬c) where I is the QF_NRA interpolant

     - for the resultion step c = resolve(c₁, c₂) where c₁ = x ∨ ? and c₂ = ¬x ∨ ?
         * if x ∈ A  then PI(c) = PI(c₁) ∨ PI(c₂)
         * if x ∈ B  then PI(c) = PI(c₁) ∧ PI(c₂)
         * if x ∈ AB then PI(c) = ite(¬x, PI(c₁), PI(c₂))

* more flexibility about the labeling function [D'Silva et. al. 10](http://link.springer.com/chapter/10.1007%2F978-3-642-11319-2_12)

     in some case an AB label can be changed into A or B (and change the strength of the interpolant).
     The labelling needs to be coherent with the subproof, i.e., if l(x) = A but x is in AB, then any f containing x must have label A.
     The trick is to defined the invariant (bound can be seen as constraints pushed on one side or the other).

* lower and upper bounds could be different for different variables

* compute the interpolant incrementally (if we want to process large proofs)

    - avoid conversion to Split

    - process the proof on the fly as much as possible, e.g, once a branch of a split is processed it can be thrown away

    - increase the stack space or use a dedicated stack to avoid overflow

* smoothing

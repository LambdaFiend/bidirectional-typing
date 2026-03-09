# VSBDT
## An informal introduction
**V**ery **S**imple **B**i**D**irectional **T**yping.

Please refer to **Jana Dunfield's** and **Neelakantan R. Krishnaswami's** survey: **Bidirectional Typing** \[2020\].

## Input for VSBDT

In order to use VSBDT, write a program into a file from within the ```programs/``` directory. The ```config.txt``` file is used for changing the path of the targeted file for using the program. The default file is ```input.in```. In order to use a file as input, define the contents of ```config.txt``` so that they respect the following format, replacing \<path\> with the actual path: ```path=\<path\>```. 

An example:

```path=programs/input.in```

## Actually running VSBDT

This should suffice:

```cabal build```

```cabal run```

## The effects of running VSBDT
It will attempt to find the file using the specified path at ```config.txt```, and if everything is correct it should show the parsed program and output it's detected type right after.

## Syntax and Semantics

| Syntax | Typing | Rule Applied |
| :----: | :----- | :----------- |
| () | Checks unit against () | unit Introduction Check |
| x | Synthesizes the type A<br>associated to x in the environment | Var Synthesis |
| \x.t | Checks t against B assuming<br> gamma extended with (x:A), and<br>(\x.t) must check against (A->B) | Arrow Introduction Check |
| t1 t2 | Synthesizes (A->B) for t1,<br>checks t2 against A and then<br>synthesizes B for (t1 t2) | Arrow Elimination Syntehsis |
| t : T | Checks t against T and then<br>synthesizes T for (t : T) | Annotation Synthesis |
| - | Additionally, if, when checking<br>against B, a rule does not<br>have a match, subsumption is<br>applied and thus synthesizes A,<br>and (A=B) must be a true statement | Subsumption Check |

| Types | Meaning |
| :---: | :------ |
| unit | The only base type, with<br>no real meaning in itself |
| T1->T2 | Type arrow, generated<br>byterm abstractions |

## Some insights

Despite how arid this particular type system may seem to be (it only posesses the unit base type, after all), it's quite useful for having a first glance at how bidirectional typing could be implemented. The core idea is that there are two modes of typing, and when a premise must employ the **checking** mode, its own inference rule requires its conclusion - that is, the premise in question - to employ the same mode (in this case, checking). And vice-versa for **synthesis**.

## Report any bugs
Do not forget to report any bugs. I'll be very glad to listen to any complaints.

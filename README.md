# SFBDT

**S**ystem **F** **B**i**D**irectional **T**yping.

## An informal introduction

Please refer to **Jana Dunfield's** and **Neelakantan R. Krishnaswami's** paper: **Complete and Easy Bidirectional Typechecking
for Higher-Rank Polymorphism** \[2013\].

This was made for a university project under the guidance of **Professor Mário Florido**.

## Actually running SFBDT

This should suffice:

```cabal build```

```cabal run```

## Using SFBDT

SFBDT is a REPL. So, in order to use it, you may either write into a file inside the ```programs``` directory, or directly use the REPL's mid-execution features.

## Understanding SFBDT's Language

Check the referenced article, the example inputs from ```programs/default_tests.txt``` and try using the REPL for a little while.

## Syntax and Semantics

Regarding the "Typing" and the "Meaning" columns, one for each syntax construct, I might add them later on. This system is far more complex than that of VSBDT, which means I'm unlikely to be able to simply describe the typing of each contruct using a small table. I will have to think this through first.

| Syntax |
| :----: |
| () |
| x |
| \x.t |
| t1 t2 |
| t : T |
| - |

| Types | Meaning |
| :---: | :------ |
| unit | The only base type, with<br>no real meaning in itself |
| T1->T2 | Type arrow, generated<br>by term abstractions |
| t | A type variable for<br>polymorphic as well as<br>monomorphic types, which<br> may contain upper and lower<br>case letters, and be include any<br>number of apostrophes at<br>the end of itself |
| ∀t.T' | Used for polymorphic types,<br>it binds type variables<br>which themselves are monomorphic<br>although instantiable; also,<br>other syntactic forms for ∀<br>are forall and All |

## REPL's Commands

Most of the commands are simple and related in purpose. The table is dense because there are multiple configurations for the same thing. It was not well thought out, but serves its purpose. I hope this was not too much of a hurdle.

| Command(s) | Usage | Description |
|------------|-------|------------|
| *(All commands)* | — | Command names (the first token of the command) are not case sensitive. |
| :var, :v, :assign, :a | :v \<var_name\> | Assign a written term to <var_name>. |
| :type, :ty, :t | :t \<var_name\> | Show the type of the term assigned to <var_name>. |
| :eval, :ev, :e | :e \<var_name\> | Fully evaluate the term from <var_name>. |
| :evaln, :evn, :en | :en <number_of_steps> <var_name> | Evaluate (<number_of_steps>) n-steps the term from <var_name>. |
| :help, :h, :? | :h | Display information regarding the commands. |
| :show, :sh, :s | :s \<var_name\> | Show the term assigned to <var_name>. |
| :var, :v, :assign, :a, (+ eval) | :v <var_name1> :ev <var_name2> | Evaluate from the current environment (given <var_name2>) and store into <var_name1>. |
| :var, :v, :assign, :a, (+ evaln) | :v <var_name1> :evn <number_of_steps> <var_name2> | Evaluate n-steps from the current environment and store into <var_name1>. |
| :load, :l | :l \<file_path\> | Load terms from file at <file_path>, assigned as `<var_name> := <expression>`, and load into the environment. |
| :v?, :vars | :v? | Show the first page (10 environment variables) if a number is not specified. |
| :v?, :vars | :v? \<number\> | Show the <number>'th page (containing 10 environment variables' names). |
| :m, :mv, :move | :mv \<var_name1\> \<var_name2\> | Store the contents of <var_name2> into <var_name1>. |
| :q, :quit | :q | Close the REPL. |
| :te, :tenv, :typeenv | :typeenv | Attempt to type all variables in the environment. |
| :ee, :eenv, :evalenv | :evalenv | Attempt to evaluate all variables in the environment. |
| :c, :ce, :cenv, :clear, :clearenv | :c | Clear the environment (no variables accessible until new ones are added). |
| :av?, :allvars | :av? | Show all variables in the environment. |
| :showenv, :showe, :senv, :se | :se | Show the environment. |
| :showenv, :showe, :senv, :se | :se \<page_number\> | Show a specific environment page. |
| :te, :tenv, :typeenv | :te \<page_number\> | Type a specific environment page. |
| :ee, :eenv, :evalenv | :ee \<page_number\> | Evaluate a specific environment page. |
| \<program\> | \<program\> | Shows, then Types and then Evaluates the given program/term. |
| *(Environment pages)* | — | Page numbers start at 1. |

In contrast to YALCI's REPL, this one does not include desugaring, as it would hold no relevance to the language and its respective type system.

## Some insights of mine

Just like VSBDT's, this bidirectional type system only has the unit base type. Indeed, this means the degree of expressivity of this type system depends entirely on how we can use System F to encode meaningful structures.

No type annotations are required whatsoever, as let-bindings are not included in the implementation. Thus, one may wonder what purpose annotations serve: mainly, it allows for the downcasting of a term's type.

Furthermore, this polymorphism is impredicative. In practice, it means that, unlike System F, this system won't allow an abstracted variable (which occurs multiple times) to be instantiated to more than one unique type. Now, technically, it means that we can't instantiate type variables to polymorphic types, precisely because type variables must be monomorphic. There are some examples of this inside the ```programs/default_tests.txt``` file.

Despite this restriction, this type system manages to be quite appealing. Its degree of expressivity does not need to be pushed further in a handful of scenarios, and it brings great comfort to the programmer for not having to write any annotations. However, in constrast, it should be noted that a lack of let-bindings as well as of fixed point iterators makes the act of programming less intuitive and even too restrictive.

## Report any bugs
Do not forget to report any bugs. I'll be very glad to listen to any complaints.

I do have (and will) to get better at Haskell.

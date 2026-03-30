# LTIBDT

**L**ocal **T**ype **I**nference **B**i**D**irectional **T**yping.

## An informal introduction

Please refer to **Benjamin Pierce's** and **David Turner's** article: **Local Type Inference** \[2000\].

The other branches of this repository are different implementations of bidirectional typing. They are as worthwhile as this one, especially SFBDT.

This was made for a university project under the guidance of **Professor Mário Florido**.

## Actually running LTIBDT

This should suffice:

```cabal build```

```cabal run```

## Using LTIBDT

LTIBDT is a REPL. So, in order to use it, you may either write into a file inside the ```programs``` directory, or directly use the REPL's mid-execution features.

## Understanding LTIBDT's Language

Check the referenced article, the example inputs from ```programs/default_tests.txt``` and try using the REPL for a little while.

## Syntax and Semantics

Regarding the Typing for each syntax construct, I might add it later on. This system is far more complex than that of VSBDT, which means I can't simply describe the typing of each contruct using a small table. I will have to think this through first.

| Syntax | Meaning |
| :----: | :------ |
| x | A term variable |
| fun\[X1,...,Xn\](x1,...,xm)t | Function definition, also<br>known as abstraction,<br>albeit uncurried |
| fun\[X1,...,Xn\](x1:T1,...,xm:Tm)t | Function definition but<br>with type annotations |
| t1 \[T1,...,Tn\] (t21,...,t2m) | Function application |
| t1 (t21,...,t2m) | Function application when the<br>type variables will be inferred |

If there are type annotations in a function definition's term arguments, they must appear in every single term argument.

| Types | Meaning |
| :---: | :------ |
| Bot | The most general type<br>in the subtyping system |
| Top | The least general type<br>in the subtyping system |
| X | A type variable for<br>polymorphic types, which<br>must begin with<br>an uppercase letter, and include any<br>number of apostrophes at<br>by the end |
| All(X1,...,Xn)(T1,...,Tm)->R | Used for polymorphic types,<br>it binds type variables and<br>describes the input types tuple<br>and the result type |

## REPL's Commands

Most of the commands are simple and related in purpose. The table is dense because there are multiple configurations for the same thing. It was not well thought out, but serves its purpose. I hope this was not too much of a hurdle.

| Command(s) | Usage | Description |
|------------|-------|------------|
| *(All commands)* | — | Command names (the first token of the command) are not case sensitive. |
| :var, :v, :assign, :a | :v \<var_name\> | Assign a written term to <var_name>. |
| :type, :ty, :t | :t \<var_name\> | Show the type of the term assigned to <var_name>. |
| :help, :h, :? | :h | Display information regarding the commands. |
| :show, :sh, :s | :s \<var_name\> | Show the term assigned to <var_name>. |
| :var, :v, :assign, :a | :v <var_name1> | Evaluate from the current environment (given <var_name2>) and store into <var_name1>. |
| :var, :v, :assign, :a | :v <var_name1> | Evaluate n-steps from the current environment and store into <var_name1>. |
| :load, :l | :l \<file_path\> | Load terms from file at <file_path>, assigned as `<var_name> := <expression>`, and load into the environment. |
| :v?, :vars | :v? | Show the first page (10 environment variables) if a number is not specified. |
| :v?, :vars | :v? \<number\> | Show the <number>'th page (containing 10 environment variables' names). |
| :m, :mv, :move | :mv \<var_name1\> \<var_name2\> | Store the contents of <var_name2> into <var_name1>. |
| :q, :quit | :q | Close the REPL. |
| :te, :tenv, :typeenv | :typeenv | Attempt to type all variables in the environment. |
| :c, :ce, :cenv, :clear, :clearenv | :c | Clear the environment (no variables accessible until new ones are added). |
| :av?, :allvars | :av? | Show all variables in the environment. |
| :showenv, :showe, :senv, :se | :se | Show the environment. |
| :showenv, :showe, :senv, :se | :se \<page_number\> | Show a specific environment page. |
| :te, :tenv, :typeenv | :te \<page_number\> | Type a specific environment page. |
| \<program\> | \<program\> | Shows, then Types and then Evaluates the given program/term. |
| *(Environment pages)* | — | Page numbers start at 1. |

In contrast to YALCI's REPL, this one does not include desugaring, as it would hold no relevance to the language and its respective type system.

## Some insights

This section will be expanded in the near future.

LTIBDT can now type! I'm likely going to add evaluation as well, let-bindings and some base types. Well, I might *only* add let-bindings.

## Report any bugs
Do not forget to report any bugs. I'll be very glad to listen to any complaints.

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
| let x = t1 in t2 | Let-binding, binds the<br>term t1 to x into<br>the context for t2, so<br>that it may be used in t2 |

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

In contrast to YALCI's REPL, this one does not include any form of desugaring or evaluation, as it would hold no relevance to the language and its respective type system (or, in the case of evaluation, the objective of making LTIBDT).

## Some insights

LTIBDT can now type! I've also added let-bindings. No other addition will be made, other than getting rid of bugs.

This implementation was significantly more challenging than that of SFBDT (SFBDT might not be a good designation anymore, as both SFBDT and LTIBDT are forms of System F - in fact, LTIBDT resembles it the most). In the paper for Local Type Inference, some details were ommited (mainly due to how obvious they are) and I have, predictably, recovered them. The code can speak for it self, but maybe I should have it stated explicitly (I'm particularly referring to the mechanism for determining the minimal substitution, if there is any).

I would say this is a rather faithful representation of partial type inference for System F, albeit with sub-types. Much can be encoded, and the requisites for having to annotate are easy to predict, for someone who has tried using the language for a short while. The main idea of LTI is that there are certain sociological aspects of functional programming (in ML, at least) from which a bidirectional type system can take. Notably, top-level type annotations are useful and common practice, but not so much for anonymous functions and even less so for arguments. It can be demonstrated how these heuristics have been pursued successfully, as, besides top-level annotations, only intricate cases require annotations. It will be possible, in many cases, to ommit the annotations of anonymous functions' arguments whenever they are an argument. In the same fashion, type arguments can be ommited. The only flaw here is that top-level-ness is not always clear, so sometimes the type system will be synthesizing and thus will not be able to infer the type of an un-annotated function. I do not believe this to be much of a problem, considering that the patterns for when it's synthesizing or checking (even if the programmer is not fully aware of the insides of the machine) is intuitive enough. Given enough practice, it becomes rather obvious, I'd say. That's how it is for most if not all programming languages. What does the programmer value? That's what shall answer the question of "Is this type system useful?". Not in absolute, though. Times change.

I would like to compare LTIBDT against SFBDT. I'm not going to do it thoroughly here, but decently enough. LTIBDT is really powerful, it allows for a great deal of expressivity. SFBDT, on the other hand, is predicative, so it does suffer from it's practical short-handedness, which means not much work/abstraction can be relayed to the language/compiler. I probably should have added let-bindings to it, as it might have shown how it can be more useful than one would care to think. I'll be taking a look at that, that's for sure, but, for now, I'll ignore its existence. When it comes to annotations, SFBDT takes the crown, as, without let-bindings (I remember reading a small section where the authors say that let-bindings change this), no annotations are necessary whatsoever. Generic code can be written (polymorphically), but it can only be instantiated with monomorphic types. Now, as for LTIBDT, the belt feels tightened considerably. One should always keep in mind that the type-checker is rigorous and so we must pay close attention to how we type our programs. This can either be good or bad. Again, it ends up being for the programmer (or someone else) to decide.

I will say a couple more things, eventually.

## Report any bugs
Do not forget to report any bugs. I'll be very glad to listen to any complaints.

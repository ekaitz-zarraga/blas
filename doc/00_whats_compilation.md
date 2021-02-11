# Introduction to compilation

The compilation can be defined as the process of converting a program written
in one language to another.

Normally, the compilation is done by a program called *compiler*. Of course,
you could do this by hand, in fact, LISP was born as a representation language
that was translated to machine instructions by a human.

This project is about the creation of a program that makes the translation, so
we are *making a compiler*.

That said, there are three languages we have to choose:

- The **source** language: this is the input of the compiler, the language we
  are translating *from*.
- The **target** language: this is the output of the compiler, the language we
  are translating *to*.
- The **implementation** language: this is the language we implement the
  compiler with. In many cases, people try to implement compilers that can
  compile themselves so they use the same language as the source, but that has
  an issue: how do you compile the compiler the first time if the compiler is
  not made yet? This is called the *bootstrapping problem*.

In our case, we are going to use *Scheme* as both *implementation* and *source*
language and we are going to *target* RISC-V assembly.

As we didn't build the compiler yet, we need to run the compiler using an
external *Scheme* implementation. We are choosing *Guile* for that, but we
could use any other.

Summary:

```
       Source     --->       COMPILER      --->        Target
      (Scheme)              (written in           (RISC-V Assembly)
                              Scheme)
```

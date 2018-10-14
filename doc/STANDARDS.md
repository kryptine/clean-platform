# Standards

The following guidelines should be adhered to when developing libraries for the
Clean Platform library collection.

## What is the purpose of Clean Platform

Clean Platform was created to have a central place where commonly used
functionality was stored so that people didn't have to look for it. All the
functionality should be available on all platforms. This means that
functionality only working on Windows has no place here. It is allowed to
simulate functionality across systems. Examples of this is the System.Process
module that offers the same API across platforms.

## Type names 

The names of types should be clear and informative, and should always start
with a capital.  If the name of a type consists of multiple words, each new
word should start with a capital.  Whenever the name is an abbreviation the
abbreviation should be written using only capitals (e.g. GUI,SQL,HTTP).

## Function names 

Function names should be written in lowerCamelCase. By starting types and
constructors with a capital and functions without one, the difference between
a constructor and a function is immediately clear for the reader of a program.
Generic function names should normally start with `g`, and the next character
should be a capital.

## Module names 

For modules, the same guidelines apply as for naming types. Names should be
informative and preferably short.

- When a library module is not meant for direct imports by end users, but
  should only used by experts in modules that for example provide a more
  friendly interface, you should prefix the name of that module with an
  underscore character (`_`) or place it in a separate `Internal` submodule.

- When a module (mainly) provides generic functions for functionality that
  could also be reasonably implemented differently, it should be prefixed with
  `Gen`.

## Argument order 

While there are no hard demands on the order in which you specify the arguments
of functions, there are two rules which make your functions easier to use and
somewhat more clear:

- State representing arguments such as the common `*World` type argument,
  should be at the end of the argument list.
- Arguments which are used as "options" in some way should be at the beginning
  of the arguments. This makes it easy to pass in options by currying.

## Comments 

A concise description of the purpose of a function and the meaning of its
arguments and result should be present in the .dcl file for all exported
functions. The documentation should not be included in the .icl file for
maintainability. Comments are specified as follows:

```clean
/**
 * This function is the identity.
 * @param Some value
 * @result The same value
 */
id :: a -> a
id x = x
```

Several JavaDoc like parameters are supported such as `@param`, `@result`,
`@type`, `@var` and `@representation`. More info about this can be found
[here](https://github.com/clean-cloogle/Cloogle#clean-documentation).
We use `@complexity` for the complexity order. Some other special fields are
used, like `@gin-icon`, but one should be reluctant with inventing new field
names. If there is a general use case, adding it can be discussed.

## Layout 

- Tabs should be used for indentation. Spaces for alignment.
- The `where` keyword should be at the same level as the parent code block.

## Exporting functions and types

Definition modules (.dcl) must be very specific about the modules they import
because everything imported in a definition module is exported as well,
increasing the chance of name collisions. To minimize the chance for
collisions, adhere to the following conventions:

- Explicitly import the types and classes you need for specifying the type
  signatures by using the `from ... import ...` notation.

- Only ever import an entire module with the `import ...` notation if you
  really truly want to re-export the entire module.

Implementation modules may import anything they like.

## Implementing class instances and generic derives 
The applicable instances for the _general_ classes should be exported in the module of the type and not of the class.
This means that for example the `Functor` instance of `Maybe` should be defined in `Data.Maybe` and not in `Data.Functor`.

For _specific_ classes the instances for types should be exported in submodules.
For example, `JSONEncode` for `Map` should be exported in `Data.Map.GenJSON` and not in `Data.Map` nor in `Text.GenJSON`.
This rule also holds for types that have multiple valid instances such as the `Monoid` for `Int`.

_general_ classes are:

  - [ ] `Functor` from `Data.Functor`
  - [ ] `Monoid, Semigroup` from `Data.Monoid`
  - [ ] `Monad` from `Control.Monad` and applicable monads from `Control.Monad.*`
  - [ ] `Applicative, Alternative` from `Control.Applicative`
  - [ ] `gEq{|*|}` from `Data.GenEq`
  - [ ] `gDefault{|*|}` from `Data.GenDefault`
  - [ ] `GenFDomain` from `Data.GenFDomain`
  - [ ] everything from `StdOverloaded`
  - [ ] ...

_specific_ classes are for example:

  - [ ] `JSONEncode, JSONDecode` from `Text.JSON`
  - [ ] ...


## OS/Architecture specific functionality
When implementing functionality that is OS or Architecture specific it is preferred to implement it for all platforms.
This means that it is preferred to define a common interface to offer the same functionality on all platforms.
However, this is not always possible (e.g. PseudoTTY support is not available on windows) and therefore this is not mandatory.

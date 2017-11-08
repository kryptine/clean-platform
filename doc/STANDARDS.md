The following guidelines should be adhered to when developing libraries for the
Clean Platform library collection.

## Type names 

The names of types should be clear and informative, and should always start
with a capital.  If the name of a type consists of multiple words, each new
word should start with a capital.  Whenever the name is an abbreviation the
abbreviation should be written using only capitals (e.g. GUI,SQL,HTTP).

## Function names 

Function names should be written in lowerCamelCase. By starting types and
constructors with a capital and, functions without one, the difference between
a constructor and a function is immediately clear for the reader of a program.

## Module names 

For modules, the same guidelines apply as for naming types. Names should be
informative and preferably short. When a library module is not meant for direct
imports by end users, but should only used by experts in modules that for
example provide a more friendly interface, you should prefix the name of that
module with an underscore character or place it in a separate Internal
submodule.

## Argument order 

While there are no hard demands on the order in which you specify the arguments
of functions, there are two rules which make your functions easier to use and
somewhat more clear:

- State representing arguments such as the common `*World` type argument,
  should be at the end of the argument list.
- Arguments which are used as "options" in some way should be at the beginning
  of the arguments. This makes it easy to pass in options by currying.

## Comments 
A concise description of the purpose of a function and the meaning of its input
and output parameters should be present in the .dcl file for all exported
functions. Comments are specified as follows:

```
/**
 * This function is the identity.
 */
id :: a -> a
id x = x
```

Several JavaDoc like parameters are supported such as `@param`, `@result`,
`@type`, `@var` and `@representation. More info about this can be found
[here](https://github.com/clean-cloogle/Cloogle#clean-documentation)

## Layout 

Tabs should be used for indentation. Spaces for alignment.
`where` clauses should not be indented.

## Exporting functions and types =

Definition modules (.dcl) must be very specific about the modules they import
because everything imported in a definition module is exported as well,
increasing the chance of name collisions. To minimize the chance for
collisions, adhere to the following conventions:

- Explicitly import the types and classes you need for specifying the type
  signatures by using the "from ... import ..." notation.

- Only ever import an entire module with the "import ..." notation if you
  really truly want to re-export the entire module.

Implementation modules may import anything they like.

## Implementing class instances and generic derives 


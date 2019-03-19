# Clean documentation
Cloogle indexes documentation of the syntax elements it stores, through
functions in `Clean.Doc`. Docblocks are comments that start with `/**` and have
a leading asterisk on every line (leading whitespace is ignored). The first
part of the docblock is taken as a general description. Below the description,
documentation fields can be added with `@`. Currently, documentation fields
should have only one line.

An example is below:

```clean
/**
 * Apply a function to every element in a list.
 *
 * @param The function
 * @param The list
 * @result The new list
 */
map :: (a -> b) [a] -> [b]
```

`@result` can be given multiple times for tuples.

For short documentation items, doclines, starting with `//*` can be used. When
documenting a constructor, or record field, they should be placed *after* the
item they document. Doclines are only supported for constructors and record
fields. For example:

```clean
/**
 * A date in the Gregorian calendar
 */
:: Date =
	{ day   :: Int  //* The day of the month, starting with 1
	, month :: Int  //* The month (January is 1)
	, year  :: Int  //* The year
	}
```

## Markup in documentation

Some simple Markdown-inspired markup is allowed in documentation:

- `` `foo` `` renders `foo` in monospaced font.
- Code blocks can be surrounded by `` ``` `` on separate lines. The start of a
  code block can indicate the language (for highlighting purposes), as in
  `` ```clean ``.
- `{{bar}}` marks `bar` as a defined entity (that can be searched for).
- Double newlines distinguish paragraphs; single newlines are ignored unless
  followed by a hyphen.

## Documentation fields

The tables below describe which fields and documentation types can be used for
different syntax elements, and what they should document. An extension, to
document test properties, is discussed below.

What fields are accepted for what syntax elements is defined by the records in
`Clean.Doc`; how they are parsed in the instances of the generic function
`docBlockToDoc`. The below is merely a convenient representation of the same
information.

|              | Description | `@param` | `@result` | `@type` | `@var` | `@representation` | `@throws` | `@complexity`
|--------------|-------------|----------|-----------|---------|--------|-------------------|-----------|--------------
| Class        | ![][y]      | ![][y]<sup>1</sup> | ![][y]<sup>1</sup> | | ![][y]          |           |
| Class member | ![][y]      | ![][y]   | ![][y]    |         |        |                   | ![][y]    | ![][y]
| Constructor  | ![][y]      |          |           |         |        |                   |           |
| Function     | ![][y]      | ![][y]   | ![][y]    |         |        |                   | ![][y]    | ![][y]
| Generic      | ![][y]      | ![][y]   | ![][y]    |         | ![][y] |                   |           |
| Instance     |             |          |           |         |        |                   |           |
| Macro        | ![][y]      | ![][y]   | ![][y]    | ![][y]<sup>2</sup> | |               |           |
| Module       | ![][y]      |          |           |         |        |                   |           |
| Record field | ![][y]      |          |           |         |        |                   |           |
| Type         | ![][y]      |          |           |         | ![][y] | ![][y], for type synonyms |   |

<sup>1: only for shorthand classes like `class zero a :: a`, where there is no
other place for the documentation of the class member.</sup>  
<sup>2: for simple macros (depending on what the type deriver in
`Clean.Types.CoclTransform` can do), Cloogle will derive the type if it is not
given.</sup>

| Field             | Description
|-------------------|-------------
| `@complexity`     | E.g. "O(n log n)".
| `@param`          | Parameters of a function(-like). Name a parameter using `@param name: description`.
| `@representation` | The representation of a synonym type.
| `@result`         | The result of a function.
| `@return`         | A deprecated synonym of `@result`.
| `@throws`         | iTasks exceptions that can be thrown.
| `@type`           | The type of a macro (without name and `::`).
| `@var`            | Type variables of types, classes and generics.

### Property documentation

With [clean-test-properties][]' `testproperties` tool, [Gast][] test programs
can be generated with properties from docblocks. For this, several additional
fields can be used, which are further documented by [clean-test-properties][].

[clean-test-properties]: https://gitlab.science.ru.nl/clean-and-itasks/clean-test-properties
[Gast]: https://gitlab.science.ru.nl/clean-and-itasks/gast

[y]: http://i.stack.imgur.com/iro5J.png

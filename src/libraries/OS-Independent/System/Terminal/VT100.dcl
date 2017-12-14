definition module System.Terminal.VT100

from Text.HTML import :: HtmlTag

/*
 * @param Number of characters per row
 * @param Number of characters per column
 * @param Number of spaces per tabstop
 * @param String containing the data
 * @return Render as HTML
 */
vt100render :: Int Int Int -> (String -> HtmlTag)

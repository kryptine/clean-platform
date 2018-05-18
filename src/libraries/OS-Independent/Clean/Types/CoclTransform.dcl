definition module Clean.Types.CoclTransform

/**
 * Functions to
 *
 * - transform types in the Clean compiler into types in TypeDef;
 * - derive types for very simple expressions from the compiler AST.
 */

from Data.Maybe import :: Maybe

from Clean.Types import class toType, class toTypeVar, class toTypeDef,
	class toTypeDefRhs, class toConstructor, class toRecordField,
	class toTypeContext, class toMaybePriority
import qualified Clean.Types as T

// Cocl frontend
from syntax import ::SymbolType, ::Type, ::TypeVar, ::ParsedSelector,
	::ParsedConstructor, ::RhsDefsOfType, ::TypeContext, ::Priority
import qualified syntax

instance toType SymbolType
instance toType Type

instance toTypeVar TypeVar

instance toTypeContext [TypeContext]
instance toTypeContext TypeContext

instance toTypeDef 'syntax'.ParsedTypeDef
instance toTypeDefRhs RhsDefsOfType
instance toConstructor ParsedConstructor
instance toRecordField ParsedSelector
instance toMaybePriority Priority

pdType :: 'syntax'.ParsedDefinition -> Maybe 'T'.Type

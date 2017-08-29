
grammar Ceylon;

compilationUnit
    : (compilerAnnotations SEMICOLON)?
          ( importDeclaration
          | moduleDescriptor
          | packageDescriptor
          | toplevelDeclaration
          | RBRACE
          )*
      EOF
    ;

toplevelDeclaration
    : compilerAnnotations declaration
    ;

moduleDescriptor
    : compilerAnnotations annotations
      MODULE
      packagePath
      (
        importNamespace
        SEGMENT_OP
        (STRING_LITERAL | packagePath)
        (
          SEGMENT_OP
          STRING_LITERAL
          (SEGMENT_OP STRING_LITERAL)?
        )?
      )?
      STRING_LITERAL?
      importModuleList
    ;

importModuleList
    : LBRACE
      (
        compilerAnnotations annotations
        (
          inferredAttributeDeclaration
        |
          importModule
        )
      )*
      RBRACE
    ;

packageDescriptor
    : compilerAnnotations annotations PACKAGE packagePath SEMICOLON
    ;

importModule
    : IMPORT
      (
        importNamespace
        SEGMENT_OP
      )?
      (
        (
          STRING_LITERAL
        |
          packagePath
        )
        (
          SEGMENT_OP
          STRING_LITERAL
          (
            SEGMENT_OP
            STRING_LITERAL
          )?
        )?
      )
      (
        STRING_LITERAL
      |
        memberName
      )?
      SEMICOLON
    ;

importNamespace
    : LIDENTIFIER
    | UIDENTIFIER
    ;

importDeclaration
    : IMPORT packagePath importElementList
    ;

importElementList
    : LBRACE
      (
      (
        importElement
      | importWildcard
      )
      (
        COMMA
        (
          importElement
        | importWildcard
        )
      )*
      )?
      RBRACE
    ;

importElement
    : compilerAnnotations
    ( importName
      (SPECIFY importName)?
      importElementList?
    )
    ;

importWildcard
    : ELLIPSIS
    ;

importName
    : memberName
    | typeName
    ;

packagePath
    : packageName (MEMBER_OP packageName)*
    ;

packageName
    : LIDENTIFIER
    | UIDENTIFIER
    ;

typeName
    : UIDENTIFIER
    ;

annotationName
    : LIDENTIFIER
    ;

memberName
    : LIDENTIFIER
    ;

memberNameDeclaration
    : memberName
    | typeName
    ;

typeNameDeclaration
    : typeName
    | memberName
    ;

enumeratedObject
    : NEW
      memberNameDeclaration?
      delegatedConstructor?
      (block | SEMICOLON)
    ;

objectDeclaration
    : OBJECT_DEFINITION
      memberNameDeclaration?
      extendedType?
      satisfiedTypes?
      (classBody | SEMICOLON)
    ;

objectExpression
    : OBJECT_DEFINITION
      extendedType?
      satisfiedTypes?
      (classBody | SEMICOLON)
    ;

voidOrInferredMethodDeclaration
    : (VOID_MODIFIER | FUNCTION_MODIFIER)
      memberNameDeclaration?
      typeParameters?
      parameters*
      typeConstraints?
      (block | functionSpecifier? SEMICOLON)
    ;

setterDeclaration
    : ASSIGN memberNameDeclaration? (block | functionSpecifier? SEMICOLON)
    ;

variableOrTuplePattern
    : tuplePattern
    | variablePattern
    ;

pattern
    : keyItemPattern
    | tuplePattern
    | variablePattern
    ;

tupleOrEntryPattern
    : keyItemPattern
    | tuplePattern
    ;

variablePattern
    : variable
    ;

tuplePattern
    : LBRACKET
      (
        variadicPattern
        (
          COMMA
          variadicPattern
        )*
      )?
      RBRACKET
    ;

variadicPattern
    : variadicVariable
	| pattern
    ;

variadicVariable
    : compilerAnnotations unionType? PRODUCT_OP memberNameDeclaration?
    ;

keyItemPattern
    : variableOrTuplePattern ENTRY_OP variableOrTuplePattern?
    ;

destructure
    : VALUE_MODIFIER tupleOrEntryPattern specifier? SEMICOLON
    ;

destructure2
    : LET LPAREN letVariable (COMMA letVariable)* RPAREN SEMICOLON
    ;

inferredAttributeDeclaration
    : VALUE_MODIFIER
      memberNameDeclaration?
      (
        (specifier | lazySpecifier)? SEMICOLON
      |
        block
      )
    ;

typedMethodOrAttributeDeclaration
    : ( variadicType
      | DYNAMIC
      )
      memberNameDeclaration
      (
          typeParameters?
          parameters+
          typeConstraints?
        (
          block
        |
          functionSpecifier?
          SEMICOLON
        )
      |
        (
          specifier
        |
          lazySpecifier
        )?
        SEMICOLON
      |
        block
      )
    ;

interfaceDeclaration
    : (
        INTERFACE_DEFINITION
      |
        DYNAMIC
      )
        typeNameDeclaration?
        typeParameters?
        caseTypes?
        satisfiedTypes?
        typeConstraints?
      (
        interfaceBody
      |
        typeSpecifier?
        SEMICOLON
      )
    ;

classDeclaration
    : CLASS_DEFINITION
      typeNameDeclaration?
      typeParameters?
      parameters?
      caseTypes?
      extendedType?
      satisfiedTypes?
      typeConstraints?
      (
        classBody
      |
        classSpecifier?
        SEMICOLON
      )
    ;

constructor
    : NEW memberNameDeclaration? parameters? delegatedConstructor? (block | SEMICOLON)
    ;

delegatedConstructor
    : EXTENDS classInstantiation?
    ;

aliasDeclaration
    : ALIAS typeNameDeclaration? typeParameters? typeConstraints? typeSpecifier? SEMICOLON
    ;

assertion
    : assertMessage ASSERT conditions SEMICOLON
    ;

block
    : LBRACE (importDeclaration | declarationOrStatement)* RBRACE
    ;

//Note: interface bodies can't really contain
//      statements, but error recovery works
//      much better if we validate that later
//      on, instead of doing it in the parser.
interfaceBody
    : LBRACE (importDeclaration | declarationOrStatement)* RBRACE
    ;

classBody
    : LBRACE (importDeclaration | declarationOrStatement)* RBRACE
    ;

extendedType
    : EXTENDS classInstantiation?
    ;

classSpecifier
    : (COMPUTE | SPECIFY) classInstantiation?
    ;

packageQualifiedClass
    : PACKAGE
      (
        MEMBER_OP
        (
          typeNameWithArguments
          ( //constructor
            MEMBER_OP
            memberNameWithArguments
          )?
        )?
      )?
   ;

unqualifiedClass
    : typeNameWithArguments
      ( //constructor:
        MEMBER_OP
        (
          memberNameWithArguments
        |
          typeNameWithArguments
          (
            MEMBER_OP
            typeNameWithArguments?
          )*
        )?
      )?
    | memberNameWithArguments
    ;

superQualifiedClass
    : SUPER MEMBER_OP (typeNameWithArguments | memberNameWithArguments)?
    ;

classInstantiation
    : ( packageQualifiedClass
	  | unqualifiedClass
	  | superQualifiedClass
      )
      positionalArguments?
    ;

satisfiedTypes
    : SATISFIES
      primaryType
      (
        (
          INTERSECTION_OP
        |
          (COMMA|UNION_OP)
        )
        primaryType
      )*
    ;

caseTypes
    : CASE_TYPES
      caseType
      (
        (
          UNION_OP
        |
          (COMMA|INTERSECTION_OP)
        )
        caseType
      )*
    ;

caseType
    : primaryType
    | memberName
    | PACKAGE MEMBER_OP memberName
    ;

abstractedType
    : ABSTRACTED_TYPE primaryType
    ;

parameters
    : LPAREN
      (
        parameterDeclarationOrRefOrPattern
        (
          COMMA
          parameterDeclarationOrRefOrPattern
        )*
      )?
      RPAREN
    ;

parameter
    : compilerAnnotations annotations parameterDeclaration
    ;

parameterDeclaration
    : (
        variadicType
      | VOID_MODIFIER
      | FUNCTION_MODIFIER
      | DYNAMIC
      | VALUE_MODIFIER
      )
      memberNameDeclaration
      (
        specifier?
      |
        typeParameters?
        parameters+
        functionSpecifier?
      )
    ;

parameterRef
    : memberName specifier?
    ;

parameterDeclarationOrRefOrPattern
    : pattern
    | parameterDeclarationOrRef
    ;

parameterDeclarationOrRef
    : parameter
    | parameterRef
    ;

typeParameters
    : SMALLER_OP
      typeParameter
      (
        COMMA
        typeParameter
      )*
      LARGER_OP
    ;

typeParameter
    : compilerAnnotations variance? typeNameDeclaration typeDefault?
    ;

variance
    : IN_OP
    | OUT
    ;

typeConstraint
    : compilerAnnotations TYPE_CONSTRAINT typeNameDeclaration? typeParameters? caseTypes? satisfiedTypes? abstractedType?
    ;

anonymousTypeConstraint
    : TYPE_CONSTRAINT typeNameDeclaration caseTypes? satisfiedTypes?
    ;

typeConstraints
    : typeConstraint+
    ;

anonymousTypeConstraints
    : anonymousTypeConstraint+
    ;

declarationOrStatement
    : compilerAnnotations
        ( destructure
        | destructure2
        | assertion
        | declaration
        | statement
        )
    ;

declaration
    : annotations
        ( classDeclaration
        | interfaceDeclaration
        | aliasDeclaration
        | objectDeclaration
        | setterDeclaration
        | voidOrInferredMethodDeclaration
        | inferredAttributeDeclaration
        | typedMethodOrAttributeDeclaration
        | constructor
        | enumeratedObject
        )
    ;

// recognize some common patterns that are unambiguously
// type abbreviations - these are not necessary, but
// help the IDE
fullQualifiedType
    : baseType (MEMBER_OP typeNameWithArguments)*
    ;

statement
    : directiveStatement
    | controlStatement
    | expressionOrSpecificationStatement
    | SEMICOLON
    ;

expressionOrSpecificationStatement
    : expression
      lazySpecifier?
      (
        SEMICOLON
      |
        COMMA
      )
    ;

directiveStatement
    : directive SEMICOLON
    ;

directive
    : returnDirective
    | throwDirective
    | breakDirective
    | continueDirective
    ;

returnDirective
    : RETURN functionOrExpression?
    ;

throwDirective
    : THROW expression?
    ;

breakDirective
    : BREAK
    ;

continueDirective
    : CONTINUE
    ;

typeSpecifier
    : (COMPUTE | SPECIFY) type?
    ;

typeDefault
    : SPECIFY type
    ;

specifier
    : SPECIFY functionOrExpression
    ;

lazySpecifier
    : COMPUTE functionOrExpression
    ;

functionSpecifier
    : (COMPUTE | SPECIFY) functionOrExpression
    ;

expression
    : assignmentExpression
    ;

base
    : nonstringLiteral
    | stringExpression
    | metaLiteral
    | enumeration
    | tuple
    | dynamicObject
    | objectExpression
    | parExpression
    | baseReferenceOrParameterized
    ;

baseReferenceOrParameterized
    : memberName typeParameters? parameters+
    | baseReference
    | selfReference (memberSelectionOperator memberName typeParameters? parameters+)?
    ;

baseReference
    : memberReference
    | typeReference
    ;

primary
    : base
      (   qualifiedReference
        | indexOrIndexRange
        | positionalArguments
        | namedArguments
      )*
    ;

qualifiedReference
    : memberSelectionOperator
      ( memberReference
      | typeReference
      | (~(LIDENTIFIER|UIDENTIFIER))
      )
    ;

memberSelectionOperator
    : MEMBER_OP
    | SAFE_MEMBER_OP
    | SPREAD_OP
    ;

enumeration
    : LBRACE declarationOrStatement* sequencedArgument? RBRACE
    ;

tuple
    : LBRACKET sequencedArgument? RBRACKET
    ;

dynamicObject
    : DYNAMIC

      (
        dynamicArguments
      |
        LBRACKET COMMA RBRACKET
      )
    ;

dynamicArguments
    : LBRACKET

      (
       namedArgument

      | anonymousArgument

      )*
      sequencedArgument?
      RBRACKET
    ;

valueCaseList
    :
      intersectionExpression
      (
        (
          COMMA
        | UNION_OP

        )
        intersectionExpression
      )*
    ;

memberReference
    : memberName typeArguments?
    ;

typeReference
    : typeName typeArguments?
    ;

indexOrIndexRange
    //TODO: move indexOperator to ElementOrRange and
    //      make this rule return ElementOrRange instead
    //      of IndexExpression, instantiating IndexExpression
    //      from the calling primary rule
    : LBRACKET

      (
        ELLIPSIS
        index
      |
        index
        (
          ELLIPSIS
        |
          RANGE_OP
          index
        | SEGMENT_OP
          index
        )?
      | functionOrExpression
      )
      RBRACKET
    ;

index
    : additiveExpression
    ;

namedArguments
    : LBRACE

      (
        namedArgument

      | anonymousArgument

      )*
      sequencedArgument?
      RBRACE
    ;

sequencedArgument
    : compilerAnnotations

        (
          comprehension
        |
          positionalArgument
        |
          spreadArgument
        )
        (
          COMMA

          (
            comprehension
          |
            positionalArgument
          |
            spreadArgument
          |
          )
        )*
    ;

namedArgument
    : compilerAnnotations
    (
      namedSpecifiedArgument
    |
      namedArgumentDeclaration
    )
    ;

namedSpecifiedArgument
    : memberName specifier? SEMICOLON
    ;

anonymousArgument
    : functionOrExpression SEMICOLON
    ;

objectArgument
    : OBJECT_DEFINITION memberNameDeclaration? extendedType? satisfiedTypes? (classBody | SEMICOLON)
    ;

voidOrInferredMethodArgument
    :
      (
        VOID_MODIFIER
      |
        FUNCTION_MODIFIER
      )
        memberNameDeclaration?
        typeParameters?
        parameters*
      (
        block
      |
        functionSpecifier?
        SEMICOLON
      )
    ;

inferredGetterArgument
    :
      VALUE_MODIFIER
      memberNameDeclaration?
      (
        block
      |
        (
          specifier
        |
          lazySpecifier
        )?
        SEMICOLON
      )
    ;

typedMethodOrGetterArgument
    : (
        type
      |
        DYNAMIC
      )
      memberNameDeclaration
      (
          typeParameters?
          parameters+
        (
          block
        |
          functionSpecifier?
          SEMICOLON
        )
      |
        (
          block
        |
          (
            specifier
          |
            lazySpecifier
          )?
          SEMICOLON
        )
      )
    ;

untypedMethodOrGetterArgument
    : memberName
      (
          parameters+
          functionSpecifier
      |
          lazySpecifier
      )
      SEMICOLON
    ;

namedArgumentDeclaration
    : objectArgument
    | typedMethodOrGetterArgument
    | voidOrInferredMethodArgument
    | inferredGetterArgument
    | untypedMethodOrGetterArgument
    ;

parExpression
    : LPAREN functionOrExpression RPAREN
    ;

positionalArguments
    : LPAREN sequencedArgument? RPAREN
    ;

positionalArgument
    : functionOrExpression
    ;

spreadArgument
    : PRODUCT_OP unionExpression
    ;

functionOrExpression
    : anonymousFunction
    | let
    | ifExpression
    | switchExpression
    | expression
    ;

let
    : letClause
    ;

letVariable
    : (pattern | variable) specifier?
    ;

letClause
    : LET
      LPAREN
      (
        letVariable
        (
          COMMA
          letVariable
        )*
      )?
      RPAREN
      conditionalBranch
    ;

switchExpression
    : switchHeader caseExpressions
    ;

caseExpressions
    : caseExpression+ elseExpression?
    ;

caseExpression
    : ELSE_CLAUSE? CASE_CLAUSE caseItemList conditionalBranch
    ;

elseExpression
    : ELSE_CLAUSE conditionalBranch
    ;

ifExpression
    : IF_CLAUSE conditions thenExpression elseExpression
    ;

conditionalBranch
    : ifExpression
    | let
    | disjunctionExpression
    ;

thenExpression
    : THEN_CLAUSE conditionalBranch
    ;

anonymousFunction
    : (
        FUNCTION_MODIFIER
      |
        VOID_MODIFIER
      )?
      typeParameters?
      parameters
      parameters*
        typeConstraints?
      (
        COMPUTE
        //TODO: should be a LazySpecifierExpression!
        functionOrExpression
      |
        block
      )
    ;

comprehension
    : forComprehensionClause
    | ifComprehensionClause
    ;

comprehensionClause
    : forComprehensionClause
    | ifComprehensionClause
    | expressionComprehensionClause
    ;

expressionComprehensionClause
    : functionOrExpression
    ;

forComprehensionClause
    : FOR_CLAUSE forIterator comprehensionClause
    ;

ifComprehensionClause
    : IF_CLAUSE conditions comprehensionClause
    ;

assignmentExpression
    : thenElseExpression
      (
        assignmentOperator
        functionOrExpression
      )?
    ;

assignmentOperator
    : SPECIFY
    | ADD_SPECIFY
    | SUBTRACT_SPECIFY
    | MULTIPLY_SPECIFY
    | DIVIDE_SPECIFY
    | REMAINDER_SPECIFY
    | INTERSECT_SPECIFY
    | UNION_SPECIFY
    | COMPLEMENT_SPECIFY
    | AND_SPECIFY
    | OR_SPECIFY
    ;

thenElseExpression
    : disjunctionExpression (thenElseOperator disjunctionExpression)*
    ;

thenElseOperator
    : ELSE_CLAUSE
    | THEN_CLAUSE
    ;

disjunctionExpression
    : conjunctionExpression (disjunctionOperator conjunctionExpression)*
    ;

disjunctionOperator
    : OR_OP
    ;

conjunctionExpression
    : logicalNegationExpression (conjunctionOperator logicalNegationExpression)*
    ;

conjunctionOperator
    : AND_OP
    ;

logicalNegationExpression
    : notOperator logicalNegationExpression
    | expressionOrMeta
    ;

notOperator
    : NOT_OP
    ;

//TODO: NOT BLESSED BY SPEC!!!
expressionOrMeta
    : modelRef
    | equalityExpression
    ;

equalityExpression
    : comparisonExpression (equalityOperator comparisonExpression)?
    ;

equalityOperator
    : EQUAL_OP
    | NOT_EQUAL_OP
    | IDENTICAL_OP
    ;

comparisonExpression
    : existenceEmptinessExpression
      ( comparisonOperator existenceEmptinessExpression
      | largerOperator existenceEmptinessExpression
      | smallerOperator existenceEmptinessExpression (smallerOperator existenceEmptinessExpression)?
      | typeOperator type
      )?
    ;

smallerOperator
    : SMALL_AS_OP
    | SMALLER_OP
    ;

largerOperator
    : LARGE_AS_OP
    | LARGER_OP
    ;

comparisonOperator
    : COMPARE_OP
    | IN_OP
    ;

typeOperator
    : IS_OP
    | EXTENDS
    | SATISFIES
    | CASE_TYPES
    ;

existenceEmptinessExpression
    : entryRangeExpression existsNonemptyOperator?
    ;

existsNonemptyOperator
    : EXISTS
    | NONEMPTY
    ;

entryRangeExpression
    : additiveExpression (rangeIntervalEntryOperator additiveExpression)?
    ;

rangeIntervalEntryOperator
    : RANGE_OP
    | SEGMENT_OP
    | ENTRY_OP
    ;

additiveExpression
    : scaleExpression (additiveOperator scaleExpression)*
    ;

additiveOperator
    : SUM_OP
    | DIFFERENCE_OP
    ;

scaleExpression
    : multiplicativeExpression (scaleOperator scaleExpression)?
    ;

multiplicativeExpression
    : unionExpression (multiplicativeOperator unionExpression)*
    ;

multiplicativeOperator
    : PRODUCT_OP
    | QUOTIENT_OP
    | REMAINDER_OP
    ;

unionExpression
    : intersectionExpression (unionOperator intersectionExpression)*
    ;

unionOperator
    : UNION_OP
    | COMPLEMENT_OP
    ;

intersectionExpression
    : negationComplementExpression (intersectionOperator negationComplementExpression)*
    ;

intersectionOperator
    : INTERSECTION_OP
    ;

negationComplementExpression
    : unaryMinusOrComplementOperator negationComplementExpression
    | exponentiationExpression
    ;

unaryMinusOrComplementOperator
    : DIFFERENCE_OP
    | SUM_OP
    ;

exponentiationExpression
    : incrementDecrementExpression (exponentiationOperator exponentiationExpression)?
    ;

exponentiationOperator
    : POWER_OP
    ;

scaleOperator
    : SCALE_OP
    ;

incrementDecrementExpression
    : prefixOperator incrementDecrementExpression
    | postfixIncrementDecrementExpression
    ;

prefixOperator
    : DECREMENT_OP
    | INCREMENT_OP
    ;

postfixIncrementDecrementExpression
    : valueExpression postfixOperator*
    ;

valueExpression
    : declarationRef
    | primary
    ;

postfixOperator
    : DECREMENT_OP
    | INCREMENT_OP
    ;

selfReference
    : THIS
    | SUPER
    | OUTER
    | PACKAGE
    ;

nonstringLiteral
    : NATURAL_LITERAL
    | FLOAT_LITERAL
    | CHAR_LITERAL
    ;

stringLiteral
    : STRING_LITERAL
    | VERBATIM_STRING
    ;

stringExpression
    : stringLiteral
    | STRING_START functionOrExpression (STRING_MID functionOrExpression)* STRING_END
    ;

typeArguments
    : SMALLER_OP
      (
        (
          variance
          type?
        |
          type
        )
        (
          COMMA
          (
            variance
            type
          |
            type
          )
        )*
      )?
      LARGER_OP
    ;

variadicType
    : unionType (PRODUCT_OP | SUM_OP )?
    | type
    ;

defaultedType
    : type SPECIFY?
    | variadicType
    ;

spreadType
    : PRODUCT_OP unionType?
    ;

tupleType
    : LBRACKET
      (
        spreadType
      |
        defaultedType
        (
          COMMA
          defaultedType
        )*
      )?
      RBRACKET
    ;

groupedType
    : SMALLER_OP type LARGER_OP
    ;

iterableType
   : LBRACE variadicType? RBRACE
   ;

type
    : typeParameters anonymousTypeConstraints? COMPUTE entryType
    | entryType
    ;

entryType
    : unionType (ENTRY_OP unionType)?
    ;

unionType
    : intersectionType ((UNION_OP intersectionType)+)?
    ;

intersectionType
    : primaryType ((INTERSECTION_OP primaryType)+)?
    ;

atomicType
    : qualifiedType
    | tupleType
    | iterableType
    ;

primaryType
    : atomicType

      (
        OPTIONAL

      | LBRACKET

        (
          NATURAL_LITERAL

        )?
        RBRACKET

      | LPAREN
          (
            spreadType
          |
            defaultedType
            (
              COMMA
              defaultedType
            )*
          )?
        RPAREN
      )*
    ;

baseType
    : typeNameWithArguments
    | groupedType
    | PACKAGE MEMBER_OP typeNameWithArguments
    ;

qualifiedType
    : baseType (MEMBER_OP typeNameWithArguments)*
    ;

typeNameWithArguments
    : typeName typeArguments?
    ;

memberNameWithArguments
    : memberName typeArguments?
    ;

annotations
    : stringLiteral? annotation*
    ;

annotation
    : annotationName (positionalArguments | namedArguments)
    ;

assertMessage
    : (stringLiteral | stringExpression)?
    ;

compilerAnnotations
    : compilerAnnotation*
    ;

compilerAnnotation
    : (
        DOLLAR
      |
        AT
      )
      annotationName
      (
          SEGMENT_OP
          stringLiteral
      )?
    ;

conditions
    : LPAREN (condition (COMMA condition)*)? RPAREN
    ;

condition
    : existsCondition
    | nonemptyCondition
    | isCondition
    | satisfiesCondition
    | booleanCondition
    ;

booleanCondition
    : functionOrExpression
    ;

existsCondition
    : NOT_OP? EXISTS
      ( letVariable
      | impliedVariable
      | expression
      | impliedVariable
      | expression
      )
    ;

nonemptyCondition
    : NOT_OP? NONEMPTY
      ( letVariable
      | impliedVariable
      | expression
      | impliedVariable
      | expression
      )
    ;

isCondition
    : NOT_OP? IS_OP type (isConditionVariable | impliedVariable)
    ;

isConditionVariable
    : memberNameDeclaration specifier
    ;

satisfiesCondition
    : SATISFIES type typeName
    ;

controlStatement
    : ifElse
    | switchCaseElse
    | whileLoop
    | forElse
    | tryCatchFinally
    | dynamic
    ;

controlBlock
    : block
    ;

dynamic
    : dynamicClause
    ;

dynamicClause
    : DYNAMIC block
    ;

ifElse
    : ifBlock elseBlock?
    ;

ifBlock
    : IF_CLAUSE conditions controlBlock
    ;

elseBlock
    : ELSE_CLAUSE
      (
        elseIf
      |
        block
      )
    ;

elseIf
    : ifElse
    ;

switchCaseElse
    : switchHeader cases
    ;

switchHeader
    : SWITCH_CLAUSE LPAREN switched? RPAREN
    ;

switched
    : specifiedVariable
    | expression
    ;

cases
    : caseBlock+ elseBlock?
    ;

caseBlock
    : ELSE_CLAUSE? CASE_CLAUSE caseItemList block
    ;

caseItemList
    : LPAREN caseItem? RPAREN
    ;

caseItem
    : isCaseCondition
    | satisfiesCaseCondition
    | matchCaseCondition
    | pattern
    | matchCaseCondition
    ;

matchCaseCondition
    : valueCaseList
    ;

isCaseCondition
    : IS_OP type MEMBER_OP?
    ;

satisfiesCaseCondition
    : SATISFIES type
    ;

forElse
    : forBlock failBlock?
    ;

forBlock
    : FOR_CLAUSE forIterator controlBlock
    ;

failBlock
    : ELSE_CLAUSE controlBlock
    ;

forIterator
    : LPAREN
	    (
	      (
	        pattern
	      |
	        variable
	      )
	      containment?
	    )?
	    RPAREN
    ;

containment
    : (
        IN_OP
      |
        SEGMENT_OP
      )
      expression?
    ;

whileLoop
    : whileBlock
    ;

whileBlock
    : WHILE_CLAUSE conditions controlBlock
    ;

tryCatchFinally
    : tryBlock catchBlock* finallyBlock?
    ;

tryBlock
    : TRY_CLAUSE
      (
        resources
        controlBlock
      |
        block
      )
    ;

catchBlock
    : CATCH_CLAUSE catchVariable controlBlock
    ;

catchVariable
    : LPAREN variable? RPAREN
    ;

finallyBlock
    : FINALLY_CLAUSE controlBlock
    ;

resources
    : LPAREN
	    (
	      resource
	      (
	        COMMA
	        resource
	      )*
	    )?
	    RPAREN
    ;

resource
    : specifiedVariable
    | expression
    ;

specifiedVariable
    : variable specifier?
    ;

variable
    : compilerAnnotations var
    ;

var
    :
      ( type
      | VOID_MODIFIER
      | FUNCTION_MODIFIER
      | VALUE_MODIFIER
      )
      memberNameDeclaration
      parameters*
    |
      memberName
      parameters*
    ;

impliedVariable
    : memberName
    ;

referencePathElement
    : typeName
    | memberName
    ;

referencePath
    : (
        referencePathElement
      |
        PACKAGE
	      MEMBER_OP
	      referencePathElement
      )
      (
        MEMBER_OP
        referencePathElement
      )*
    ;

moduleLiteral
    : MODULE packagePath?
    ;

packageLiteral
    : PACKAGE packagePath?
    ;

classLiteral
    : CLASS_DEFINITION referencePath?
    ;

interfaceLiteral
    : INTERFACE_DEFINITION referencePath?
    ;

newLiteral
    : NEW referencePath?
    ;

aliasLiteral
    : ALIAS referencePath?
    ;

typeParameterLiteral
    : TYPE_CONSTRAINT referencePath?
    ;

valueLiteral
    : (VALUE_MODIFIER | OBJECT_DEFINITION) referencePath
    ;

functionLiteral
    : FUNCTION_MODIFIER referencePath
    ;

memberPathElement
    : memberName typeArguments?
    ;

memberModelExpression
    : memberPathElement
    | PACKAGE MEMBER_OP memberPathElement
    | primaryType MEMBER_OP memberPathElement
    ;

typeModelExpression
    : type
    ;

modelExpression
    : memberModelExpression
    | typeModelExpression
    ;

metaLiteral
    : BACKTICK
    ( declarationRef
    | modelExpression
    )
    BACKTICK
    ;

modelRef
    : POWER_OP modelExpression
    ;

declarationRef
    : moduleLiteral
    | packageLiteral
    | classLiteral
    | newLiteral
    | interfaceLiteral
    | aliasLiteral
    | typeParameterLiteral
    | valueLiteral
    | functionLiteral
    ;


// Lexer

fragment
Digits
    : Digit ('_' | Digit)*
    ;

fragment
HexDigits
    : HexDigit ('_' | HexDigit)*
    ;

fragment
BinaryDigits
    : BinaryDigit ('_' | BinaryDigit)*
    ;

fragment
Exponent
    : ( 'e' | 'E' ) ( '+' | '-' )? Digit*
    ;

fragment
Magnitude
    : 'k' | 'M' | 'G' | 'T' | 'P'
    ;

fragment
FractionalMagnitude
    : 'm' | 'u' | 'n' | 'p' | 'f'
    ;

fragment FLOAT_LITERAL :;
//distinguish a float literal from
//a natural literals followed by a
//member invocation or range op
NATURAL_LITERAL
    : Digits
      (
//        ('.' ('0'..'9')) =>
        '.' Digits (Exponent|Magnitude|FractionalMagnitude)?
      | FractionalMagnitude
      | Magnitude?
      )
    | '#' HexDigits
    | '$' BinaryDigits
    ;

fragment ASTRING_LITERAL:;
fragment AVERBATIM_STRING:;

CHAR_LITERAL
    :   '\'' CharPart '\''
    ;

fragment STRING_START:;
STRING_LITERAL
    :   '"' StringPart ( '"' | '``'  ('`')? )?
    ;

fragment STRING_MID:;
STRING_END
    :   '``' StringPart ( '"' | '``'  ('`')? )?
    ;

VERBATIM_STRING
    :	'"""' (~'"' | '"' ~'"' | '""' ~'"')* ('"' ('"' ('"' ('"' '"'?)?)?)?)?
    ;

/*
 Stef: we must take 32-bit code points into account here because AntLR considers each
 character to be 16-bit like in Java, which means that it will consider a high/low surrogate
 pair as two characters and refuse it in a character literal. So we match either a code point
 pair, or a normal 16-bit code point.
*/
fragment
CharPart
    : ( ~('\\' | '\'') | EscapeSequence )*
    ;

/*
 Stef: AntLR considers each character to be 16-bit like in Java, which means that it will consider a
 high/low surrogate pair as two characters, BUT because the special characters we care about, such
 as termination quotes or backslash escapes (including unicode escapes) all fall outside the range
 of both the high and low surrogate parts of 32-bit code points character pairs, so we don't care
 because 32-bit code points, even taken as two chars, cannot match any of our special characters,
 so we can just process them as-is.
*/
fragment
StringPart
    : ( ~('\\' | '"' | '`') | ( '`' ) | EscapeSequence )*
    ;

fragment
EscapeSequence
    :   '\\'
        ( ~'{' | '{' (~'}')* '}'? )?
    ;

WS
    :   (
             ' '
        |    '\r'
        |    '\t'
        |    '\f'
        |    '\n'
        )+
    ;

LINE_COMMENT
    :   ('//'|'#!') ~('\n'|'\r')*  ('\r\n' | '\r' | '\n')?
    ;

MULTI_COMMENT
    :   '/*'
        (    ~('/'|'*')
        |    '/'
        |    '*'
        |    MULTI_COMMENT
        )*
        '*/'?
        ;

BACKTICK
    : '`'
    ;

/*
    List of keywords.

    Note that this must be kept in sync with com.redhat.ceylon.compiler.typechecker.parser.ParseUtil
*/

ABSTRACTED_TYPE
    :   'abstracts'
    ;

ALIAS
    :   'alias'
    ;

ASSEMBLY
    : 'assembly'
    ;

ASSERT
    : 'assert'
    ;

ASSIGN
    :   'assign'
    ;

BREAK
    :   'break'
    ;

CASE_CLAUSE
    :   'case'
    ;

CATCH_CLAUSE
    :   'catch'
    ;

CLASS_DEFINITION
    :   'class'
    ;

CONTINUE
    :   'continue'
    ;

DYNAMIC
    :   'dynamic'
    ;

ELSE_CLAUSE
    :   'else'
    ;

EXISTS
    :   'exists'
    ;

EXTENDS
    :   'extends'
    ;

FINALLY_CLAUSE
    :   'finally'
    ;

FOR_CLAUSE
    :   'for'
    ;

FUNCTION_MODIFIER
    :   'function'
    ;

TYPE_CONSTRAINT
    :   'given'
    ;

IF_CLAUSE
    :   'if'
    ;

IMPORT
    :   'import'
    ;

IN_OP
    :   'in'
    ;

INTERFACE_DEFINITION
    :   'interface'
    ;

IS_OP
    :   'is'
    ;

LET
    :   'let'
    ;

MODULE
    :   'module'
    ;

NEW
    :   'new'
    ;

NONEMPTY
    :   'nonempty'
    ;

OBJECT_DEFINITION
    :   'object'
    ;

CASE_TYPES
    :   'of'
    ;

OUT
    :   'out'
    ;

OUTER
    :   'outer'
    ;

PACKAGE
    :   'package'
    ;

RETURN
    :   'return'
    ;

SATISFIES
    :   'satisfies'
    ;

SUPER
    :   'super'
    ;

SWITCH_CLAUSE
    :   'switch'
    ;

THEN_CLAUSE
    :   'then'
    ;

THIS
    :   'this'
    ;

THROW
    :   'throw'
    ;

TRY_CLAUSE
    :   'try'
    ;

VALUE_MODIFIER
    :   'value'
    ;

VOID_MODIFIER
    :   'void'
    ;

WHILE_CLAUSE
    :   'while'
    ;

ELLIPSIS
    :   '...'
    ;

RANGE_OP
    :   '..'
    ;

SEGMENT_OP
    :   ':'
    ;

MEMBER_OP
    :   '.' ;

LPAREN
    :   '('
    ;

RPAREN
    :   ')'
    ;

LBRACE
    :   '{'
    ;

LBRACKET
    :   '['
    ;

RBRACKET
    :   ']'
    ;

SEMICOLON
    :   ';'
    ;

COMMA
    :   ','
    ;

SPECIFY
    :   '='
    ;

COMPUTE
    :   '=>'
    ;

SAFE_MEMBER_OP
    :   '?.'
    ;

OPTIONAL
    :    '?'
    ;

NOT_OP
    :   '!'
    ;

COMPLEMENT_OP
    :   '~'
    ;

EQUAL_OP
    :   '=='
    ;

IDENTICAL_OP
    :   '==='
    ;

AND_OP
    :   '&&'
    ;

OR_OP
    :   '||'
    ;

INCREMENT_OP
    :   '++'
    ;

DECREMENT_OP
    :   '--'
    ;

SUM_OP
    :   '+'
    ;

DIFFERENCE_OP
    :   '-'
    ;

SPREAD_OP
    :    '*.'
    ;

SCALE_OP
    :    '**'
    ;

PRODUCT_OP
    :   '*'
    ;

QUOTIENT_OP
    :   '/'
    ;

INTERSECTION_OP
    :   '&'
    ;

UNION_OP
    :   '|'
    ;

REMAINDER_OP
    :   '%'
    ;

NOT_EQUAL_OP
    :   '!='
    ;

LARGER_OP
    :   '>'
    ;

SMALLER_OP
    :   '<'
    ;

LARGE_AS_OP
    :
        '>='
    |   '>' //{ $type=LARGER_OP; }
    ;

SMALL_AS_OP
    :   '<='
    ;

ENTRY_OP
    :   '->'
    ;

COMPARE_OP
    :   '<=>'
    ;

POWER_OP
    :    '^'
    ;

ADD_SPECIFY
    :   '+='
    ;

SUBTRACT_SPECIFY
    :   '-='
    ;

MULTIPLY_SPECIFY
    :   '*='
    ;

DIVIDE_SPECIFY
    :   '/='
    ;

INTERSECT_SPECIFY
    :   '&='
    ;

UNION_SPECIFY
    :   '|='
    ;

COMPLEMENT_SPECIFY
    :   '~='
    ;

REMAINDER_SPECIFY
    :   '%='
    ;

AND_SPECIFY
    :   '&&='
    ;

OR_SPECIFY
    :   '||='
    ;

DOLLAR
    :   '$'
    ;

AT
    :   '@'
    ;

fragment
LIDENTIFIER :;
fragment
PIDENTIFIER :;
fragment
AIDENTIFIER :;

UIDENTIFIER
    :   IdentifierStart IdentifierPart*
    |   UIdentifierPrefix IdentifierPart+
    |   LIdentifierPrefix IdentifierPart+
    ;

fragment
IdentifierStart
    :   '_'
    |   Letter
    ;

fragment
LIdentifierPrefix
    : '\\i'
    ;

fragment
UIdentifierPrefix
    : '\\I'
    ;

fragment
IdentifierPart
    :   '_'
    |   Digit
    |   Letter
    ;

fragment
Letter
    : 'a'..'z'
    | 'A'..'Z'
    | '\u0080'..'\uffff'
    ;

fragment
Digit
    : '0'..'9'
    ;

fragment
HexDigit
    : '0'..'9' | 'A'..'F' | 'a'..'f'
    ;

fragment
BinaryDigit
    : '0'|'1'
    ;

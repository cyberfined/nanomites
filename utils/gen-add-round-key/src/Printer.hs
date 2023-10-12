module Printer
    ( AddRoundKeyCode(..)
    , GetIvCode(..)
    , printCode
    ) where

import           Data.Text          (Text)
import           Data.Text.Encoding (StrictBuilder, strictBuilderToText,
                                     textToStrictBuilder)
import           Numeric            (showHex)

import           Expr

import qualified Data.Text          as Text

newtype AddRoundKeyCode f = AddRoundKeyCode { addRoundKeyCode :: f Stmt }

newtype GetIvCode f = GetIvCode { getIvCode :: f Stmt }

printCode :: Foldable f => AddRoundKeyCode f -> GetIvCode f -> Text
printCode AddRoundKeyCode{..} GetIvCode{..} = strictBuilderToText $  header
                                                                  <> getIv
                                                                  <> addRoundKey
  where header = foldMap textToStrictBuilder
            [ "INLINE\n"
            , "static inline uint32_t sgn32(uint32_t n) {\n"
            , "    return (-n>>31)-(n>>31);\n"
            , "}\n\n"
            , "INLINE\n"
            , "static inline uint32_t guard(uint32_t n) {\n"
            , "    uint32_t a = sgn32(n);\n"
            , "    return ((a & 1) | (a >> 31)) * 32;\n"
            , "}\n\n"
            , "INLINE\n"
            , "static inline int64_t sgn64(uint64_t n) {\n"
            , "    uint64_t s = 63;\n"
            , "    return (-n>>s)-(n>>s);\n"
            , "}\n\n"
            , "INLINE\n"
            , "static inline uint32_t address_guard(uint64_t addr, uint64_t begin, \
                  \uint64_t end) {\n"
            , "    uint64_t a = sgn64((2 + sgn64(addr - begin)) * \
                      \(2 + sgn64(end - addr)) - 4);\n"
            , "    uint64_t s = 63;\n"
            , "    a >>= s;\n"
            , "    return a * 33;\n"
            , "}\n\n"
            , "INLINE\n"
            , "static inline uint32_t rotl(uint32_t x, unsigned int n) {\n"
            , "    const unsigned int mask = (CHAR_BIT*sizeof(x)-1);\n"
            , "    n &= mask;\n"
            , "    return (x<<n) | (x>>( (-n)&mask ));\n"
            , "}\n\n"
            , "INLINE\n"
            , "static inline uint32_t rotr(uint32_t x, unsigned int n) {\n"
            , "    const unsigned int mask = CHAR_BIT * sizeof(x) - 1;\n"
            , "    n &= mask;\n"
            , "    return (x >> n) | (x << (-n & mask));\n"
            , "}\n\n"
            ]
        getIvHeader = foldMap textToStrictBuilder
            [ "INLINE\n"
            , "static inline uint32_t get_iv(uint8_t *_state, uint64_t addr) {\n"
            , "    uint32_t *state = (uint32_t*)_state;\n"
            ]
        getIv =  getIvHeader
              <> stmtsBuilder getIvCode
              <> textToStrictBuilder "    return size;\n}\n\n"
        addRoundKeyHeader = foldMap textToStrictBuilder
            [ "INLINE\n"
            , "static inline void AddRoundKey(state_t *_state, uint32_t round) {\n"
            , "    uint32_t *state = (uint32_t*)_state;\n"
            , "    uint64_t addr = (uint64_t)state;\n"
            ]
        addRoundKey =  addRoundKeyHeader
                    <> stmtsBuilder addRoundKeyCode
                    <> textToStrictBuilder "}\n"

stmtsBuilder :: Foldable f => f Stmt -> StrictBuilder
stmtsBuilder stmts = foldMap stmtBuilder stmts
  where stmtBuilder :: Stmt -> StrictBuilder
        stmtBuilder (Define typ var expr) =  tab <> typeBuilder typ <> space
                                          <> varBuilder var <> eq
                                          <> exprBuilder expr <> newline
        stmtBuilder (Assign var expr) = tab <> varBuilder var <> eq
                                     <> exprBuilder expr <> newline

        exprBuilder :: Expr -> StrictBuilder
        exprBuilder (Var var) = varBuilder var
        exprBuilder (Int32 i32) =  textToStrictBuilder "0x"
                                <> textToStrictBuilder (Text.pack $ showHex i32 "")
        exprBuilder (Int64 i64) = textToStrictBuilder "0x"
                               <> textToStrictBuilder (Text.pack $ showHex i64 "")
        exprBuilder (Binop op e1 e2) = binopBuilder op (exprBuilder e1) (exprBuilder e2)
        exprBuilder (Guard e) =  textToStrictBuilder "guard("
                              <> exprBuilder e
                              <> textToStrictBuilder ")"
        exprBuilder (AddressGuard addr begin end) = textToStrictBuilder "address_guard("
                                                 <> exprBuilder addr
                                                 <> textToStrictBuilder ","
                                                 <> exprBuilder begin
                                                 <> textToStrictBuilder ","
                                                 <> exprBuilder end
                                                 <> textToStrictBuilder ")"

        varBuilder :: Var -> StrictBuilder
        varBuilder (VarName name) = textToStrictBuilder name
        varBuilder (VarState idx) = foldMap textToStrictBuilder [ "state["
                                                                , Text.pack $ show idx
                                                                , "]"
                                                                ]
        varBuilder VarRound       = textToStrictBuilder "round"
        varBuilder VarAddress     = textToStrictBuilder "addr"

        typeBuilder :: Type -> StrictBuilder
        typeBuilder Uint32 = textToStrictBuilder "uint32_t"
        typeBuilder Uint64 = textToStrictBuilder "uint64_t"

        binopBuilder :: Binop -> StrictBuilder -> StrictBuilder -> StrictBuilder
        binopBuilder Add e1 e2 = parens (e1 <> textToStrictBuilder " + "  <> e2)
        binopBuilder Mul e1 e2 = parens (e1 <> textToStrictBuilder " * "  <> e2)
        binopBuilder Shl e1 e2 = parens (e1 <> textToStrictBuilder " << " <> e2)
        binopBuilder Shr e1 e2 = parens (e1 <> textToStrictBuilder " >> " <> e2)
        binopBuilder Xor e1 e2 = parens (e1 <> textToStrictBuilder " ^ "  <> e2)
        binopBuilder And e1 e2 = parens (e1 <> textToStrictBuilder " & "  <> e2)
        binopBuilder Rotl e1 e2 =  textToStrictBuilder "rotl("
                                <> e1
                                <> textToStrictBuilder ", "
                                <> e2
                                <> textToStrictBuilder ")"
        binopBuilder Rotr e1 e2 =  textToStrictBuilder "rotr("
                                <> e1
                                <> textToStrictBuilder ", "
                                <> e2
                                <> textToStrictBuilder ")"

        parens :: StrictBuilder -> StrictBuilder
        parens b = textToStrictBuilder "(" <> b <> textToStrictBuilder ")"

        newline :: StrictBuilder
        newline = textToStrictBuilder ";\n"

        eq :: StrictBuilder
        eq = textToStrictBuilder " = "

        tab :: StrictBuilder
        tab = textToStrictBuilder "    "

        space :: StrictBuilder
        space = textToStrictBuilder " "

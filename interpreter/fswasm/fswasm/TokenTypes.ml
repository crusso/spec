
// FSYACC doesn't like anonymous token types, so we name 'em
type CONSTType = string Source.phrase -> Ast.instr' * Values.value
type LOADType = int option -> Memory.offset -> Ast.instr'
type STOREType = int option -> Memory.offset -> Ast.instr'


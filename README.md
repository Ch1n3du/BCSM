# ByteCode Stack Machine (BCSM)
---

This is an interpreter for a very simple ByteCode.

## Running the Examples

To run the examples:
```
> stack run examples/loop.bc
```

## StackMachine Description

The StackMachine has a Stack, an Enviroment (Memory), a vector of Instructions and three special purpose registers:
- **The Program Counter (pc):** Points to the current instruction to be executed.
- **The Accumulator:** Is used for evaluating conditional jumps, can act as general purpose register.
- **The Loop Register:** Stores the address of an instruction for conditional jumps.

## ByteCode Syntax

Short summary of the ByteCode syntax.

### Stack Op-Codes

```
// This is a comment.
LOAD_VAL    // Loads a value onto the stack.
RETURN_VAL  // Returns Stack[0].
ADD         // Stack[0] = Stack[0] + Stack[1]
SUB         // Stack[0] = Stack[0] - Stack[1]
DIV         // Stack[0] = Stack[0] / Stack[1]
MUL         // Stack[0] = Stack[0] * Stack[1]
```

### Enviroment OpCodes

```
WRITE_VAR _identifier_   // Writes the value at the top of the stack to the enviroment.
READ_VAR _identifier_    // Loads the variable in the enviroment.
```

### Register OpCodes

```
READ_PC  // LOAD_VAL PC
LOAD_PC  // PC = Stack[0]
READ_AC  // LOAD_VAL AC
LOAD_AC  // AC = Stack[0]
READ_LR  // LOAD_VAL LR
LOAD_LR  // LR = Stack[0]
```

### Jump OpCodes

```
JE   // if AC == 0 then PC == LR  
JG   // if AC > 0  then PC == LR  
JGE  // if AC >= 0 then PC == LR  
JL   // if AC < 0  then PC == LR  
JLE  // if AC <= 0 then PC == LR  
```
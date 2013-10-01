package assembler

type Assembler struct {
	line, col int
}

func (asm *Assembler) emit(opcode int, name string) {
}

func (asm *Assembler) at(loc int) {
}

func (asm *Assembler) flushWord() {
}

func (asm *Assembler) defName(name string) {
}

func (asm *Assembler) enqueue(lit int) {
}
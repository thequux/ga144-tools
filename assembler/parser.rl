// Go code
package assembler;

%%{
machine f18a;

action ZeroLit {
	lit = 0;
}
action HexChar {
	if fc < '9' {
		lit = lit * 16 + fc - '0';
	} else {
		lit = lit * 16 + (fc & 0xFD) - 'A' + 10;
	}
}
action DecChar {
	lit = lit * 10 + fc - '0';
}
action OctChar {
	lit = lit * 8 + fc - '0';
}
action BinChar {
	lit = lit * 2 + fc - '0';
}

nl = "\n" %{self.line++; self.col = 0};
wsp = [ \t] | nl ;

lit =	( "0x" ( [0-9A-Za-z] )+ $HexChar
	| '0o' ( [0-7] )+ $OctChar
	| '0b' ( [01] )+ $BinChar
	| ( [1-9][0-9]* ) $DecChar
	) >ZeroLit ;


# litp = ( wsp* '[' lit ']' )? >ZeroLit ;

name = [A-Za-z_][A-Za-z_0-9]* ;

opcode  = 
	( ';' 	%{opc = 0;}
	| "ex" 	%{opc = 1;}
	| ';' name 	%{opc = 2;} # Note that there is no space
	                            # between the ';' and the name
	| name		%{opc = 3;}
	| "unext" 	%{opc = 4;}
	| "next"	%{opc = 5;}
	| "if"		%{opc = 6;}
	| "-if"		%{opc = 7;}
	| "@p"		%{opc = 8;} # optional literal
	| "@+"		%{opc = 9;}
	| "@b"		%{opc = 10;}
	| "@"		%{opc = 11;}
	| "!p"	 	%{opc = 12;} # optional literal
	| "!+"		%{opc = 13;}
	| "!b"		%{opc = 14;}
	| "!"		%{opc = 15;}
	| "+*"		%{opc = 16;}
	| "2*"		%{opc = 17;}
	| "2/"		%{opc = 18;}
	| "~"		%{opc = 19;}
	| "+"		%{opc = 20;}
	| "and"		%{opc = 21;}
	| "or"		%{opc = 22;}
	| "drop"	%{opc = 23;}
	| "dup"		%{opc = 24;}
	| "pop"		%{opc = 25;}
	| "over"	%{opc = 26;}
	| "a"		%{opc = 27;}
	| "."		%{opc = 28;}
	| "push"	%{opc = 29;}
	| "b!"		%{opc = 30;}
	| "a!"		%{opc = 31;}
	) %{self.emit(opc, name);};

directive =
	( "$flush"		%{self.flushWord();}
	| "$at(" lit ')'  	%{ self.at(lit);}
	| '[' lit ']' 		%{self.enqueue(lit);}
	| ':' wsp* name		%{self.defName(name);}
	);

comment = "#" [^\r\n]* nl ;

program := (wsp* (directive | opcode)
	    (wsp+ (directive | opcode) )*
	    wsp*) ${self.col++};

write data;
}%%

func (self *Assembler) RunPass(data []byte) error {
	var cs int
	
	%% write init;

	p := 0
	pe := len(data)
	eof := pe
	stack := make([]int, 10)
	top := 0

	var act int
	var ts, te int

	%% write exec;
}


		
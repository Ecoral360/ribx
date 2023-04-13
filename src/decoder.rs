pub const DEFAULT_INDENT_LEVEL: usize = 2;
pub static mut INDENT_LEVEL: usize = DEFAULT_INDENT_LEVEL;

fn indent_level() -> usize {
    unsafe { INDENT_LEVEL }
}


#[derive(Debug)]
pub struct SymbolTable {
    symbols: Rib, // linked list of symbols
}

impl SymbolTable {
    fn get(&self, index: usize) -> &Obj {
        let mut rib = &self.symbols;
        for _ in 0..index {
            rib = match &rib.1 {
                Obj::Rib(rib) => rib,
                _ => panic!("Invalid symbol table"),
            }
        }
        &rib.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rib(Obj, Obj, Obj);

impl Rib {
    pub fn next_op(&self) -> Obj {
        self.2.clone()
    }

    pub fn get_field1(&self) -> Obj {
        self.1.clone()
    }

    pub fn get_field0(&self) -> Obj {
        self.0.clone()
    }

    /// returns: (value popped, new value)
    pub fn pop_rib(rib: &Self) -> (Obj, Rib) {
        if *rib == NIL {
            panic!("Cannot pop from NIL");
        }

        if let Obj::Rib(new_stack) = rib.1.clone() {
            (rib.0.clone(), *new_stack)
        } else {
            (rib.0.clone(), NIL)
        }
    }

    /// returns: (value popped, new value)
    pub fn pop_op(rib: &Self) -> (Obj, Rib) {
        if *rib == NIL {
            panic!("Cannot pop from NIL");
        }

        if let Obj::Rib(new_stack) = rib.2.clone() {
            (rib.0.clone(), *new_stack)
        } else {
            (rib.0.clone(), NIL)
        }
    }

    pub fn deep_len(&self) -> usize {
        if *self == NIL {
            return 0;
        }

        let mut len = 1;
        let mut rib = self.clone();
        if rib.0 == Obj::Number(4) {
            // it is a 'if'
            len += rib.1.as_rib().unwrap().deep_len();
        }
        while let Obj::Rib(new_rib) = rib.2.clone() {
            len += 1;
            rib = *new_rib.clone();
            if rib.0 == Obj::Number(4) {
                // it is a 'if'
                len += rib.1.as_rib().unwrap().deep_len();
            }
        }
        len
    }
}

pub const NIL: Rib = Rib(Obj::Number(0), Obj::Number(0), Obj::Number(5));

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Obj {
    Number(i32),
    Rib(Box<Rib>),
}

impl Obj {
    pub fn rib(rib: Rib) -> Self {
        Self::Rib(Box::new(rib))
    }

    pub fn as_rib(&self) -> Result<Rib, String> {
        match self {
            Obj::Rib(rib) => Ok(*rib.clone()),
            _ => Err("Not a rib".into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum OpRibRepr {
    Jump {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The index of the symbol representing the function to tail-call
        slot_idx: i32,
    },
    Call {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The index of the symbol representing the function to call
        slot_idx: i32,
    },
    Set {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The index of the symbol that will be set
        slot_idx: i32,
    },
    Get {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The index of the symbol containing the value to push on the stack
        slot_idx: i32,
    },
    Const {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The object to push on the stack
        object: Obj,
    },
    ClosureConst {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The closure to push on the stack
        closure: Obj,
    },
    If {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
        /// The true branch
        true_branch: Obj,
        /// The false branch
        false_branch: Obj,
    },
    StartOfProgram {
        /// The chars in the ribn that makes this operation
        ribn_chars: String,
    },
}

impl OpRibRepr {
    fn get_ribn_chars(&self) -> String {
        match self {
            OpRibRepr::Jump { ribn_chars, .. } => ribn_chars,
            OpRibRepr::Call { ribn_chars, .. } => ribn_chars,
            OpRibRepr::Set { ribn_chars, .. } => ribn_chars,
            OpRibRepr::Get { ribn_chars, .. } => ribn_chars,
            OpRibRepr::Const { ribn_chars, .. } => ribn_chars,
            OpRibRepr::ClosureConst { ribn_chars, .. } => ribn_chars,
            OpRibRepr::If { ribn_chars, .. } => ribn_chars,
            OpRibRepr::StartOfProgram { ribn_chars } => ribn_chars,
        }
        .clone()
    }

    fn to_string(
        &self,
        symbol_table: &SymbolTable,
        with_ribn_chars: bool,
    ) -> String {
        let ribn = if with_ribn_chars {
            format!(
                "\n{}|--> {}",
                " ".repeat(indent_level()),
                self.get_ribn_chars()
            )
        } else {
            String::new()
        };
        let op_str = match self {
            OpRibRepr::Jump {
                ribn_chars,
                slot_idx,
            } => format!(
                "[jump call] push call( st<{}>{{{}}} ); jump",
                slot_idx,
                symbol_table.get(*slot_idx as usize).to_string(),
            ),
            OpRibRepr::Call {
                ribn_chars,
                slot_idx,
            } => format!(
                "[call] push call( st<{}>{{{}}} )",
                slot_idx,
                symbol_table.get(*slot_idx as usize).to_string(),
            ),
            OpRibRepr::Set {
                ribn_chars,
                slot_idx,
            } => format!(
                "[set] st<{}>{{{}}} ← pop()",
                slot_idx,
                symbol_table.get(*slot_idx as usize).to_string(),
            ),
            OpRibRepr::Get {
                ribn_chars,
                slot_idx,
            } => format!(
                "[get] push st<{}>{{{}}}",
                slot_idx,
                symbol_table.get(*slot_idx as usize).to_string(),
            ),
            OpRibRepr::Const { ribn_chars, object } => {
                format!("[const] push {}", object.to_string())
            }
            OpRibRepr::ClosureConst {
                ribn_chars,
                closure,
            } => format!(
                "[closure const] push procedure (env: {})",
                closure.as_rib().unwrap().1.to_string()
            ), //, closure.to_string()),
            OpRibRepr::If {
                ribn_chars,
                true_branch,
                false_branch,
            } => format!(
                "[if] pop()",
                //op_rib_to_string(&true_branch.as_rib().unwrap()),
                //op_rib_to_string(&false_branch.as_rib().unwrap()),
            ),
            OpRibRepr::StartOfProgram { ribn_chars } => {
                "[closure const] procedure(env: nil) {{ program }}".to_owned()
            }
        };
        format!("{}{}\n", op_str, ribn)
    }

    fn vec_to_string(
        ops: &Vec<OpRibRepr>,
        symbol_table: &SymbolTable,
        with_ribn_chars: bool,
    ) -> Vec<String> {
        let mut vectors = vec![];
        let mut branches_stack: Vec<usize> = vec![];
        for (i, op) in ops.iter().enumerate() {
            vectors.push(op.to_string(symbol_table, with_ribn_chars));
            match op {
                OpRibRepr::Jump { .. } => match branches_stack.pop() {
                    Some(line) => {
                        (line..=i).into_iter().for_each(|idx| {
                            vectors[idx] = format!(
                                "{}{}",
                                " ".repeat(indent_level()),
                                vectors[idx].replace(
                                    "\n",
                                    format!("\n{}", " ".repeat(indent_level()).as_str()).as_str()
                                )
                            );
                        });
                    }
                    _ => {}
                },
                OpRibRepr::ClosureConst { closure, .. } => {
                    branches_stack.push(i + 1);
                }
                OpRibRepr::If { .. } => {
                    branches_stack.push(i + 1);
                }
                _ => {}
            }
        }
        vectors
    }
}

const SHORT_OP: [i32; 6] = [20, 30, 0, 10, 11, 4];
const OP_JUMP: i32 = 0;
const OP_CALL: i32 = 1;
const OP_SET: i32 = 2;
const OP_GET: i32 = 3;
const OP_CONST: i32 = 4;
const OP_CLOSURE_CONST: i32 = 5;
const OP_IF: i32 = 6;
const OP_RANGES: [i32; 7] = [0, 23, 56, 59, 72, 86, 91];

type PC = Rib;

pub struct Decoder {
    ribn: String,
    pos: usize,
}

impl Decoder {
    pub fn new(ribn: String) -> Decoder {
        Decoder { ribn, pos: 0 }
    }

    fn code_len(&self) -> (usize, usize) {
        self.ribn
            .split_once(';')
            .map_or((0, 0), |(a, b)| (a.len(), b.len()))
    }

    fn get_byte(&mut self, accumulator: &mut String) -> i32 {
        let code = (&self.ribn.chars().nth(self.pos)).unwrap();
        self.pos += 1;
        accumulator.push(code);
        code as i32
    }

    fn get_code(&mut self, accumulator: &mut String) -> i32 {
        let byte: i32 = self.get_byte(accumulator) - 35;
        if byte < 0 {
            57
        } else {
            byte
        }
    }

    fn get_int(&mut self, total_46: i32, accumulator: &mut String) -> i32 {
        let next_int = self.get_code(accumulator) as i32;
        let total_46 = total_46 * 46;
        if next_int < 46 {
            next_int + total_46
        } else {
            self.get_int(next_int + total_46 - 46, accumulator)
        }
    }

    fn op_idx_to_rvm_op(op_idx: i32) -> Obj {
        Obj::Number(match op_idx {
            0 | 1 => 0,
            2 => 1,
            3 => 2,
            4 | 5 => 3,
            6 => 5,
            _ => unreachable!(),
        })
    }

    pub fn decode(&mut self, symbol_table: &mut SymbolTable) -> (PC, Vec<String>) {
        let mut op_stack = NIL;
        let mut op_repr_stack = Vec::new();
        let (sym_table_len, code_len) = self.code_len();

        println!("Sym table len: {}", sym_table_len);
        println!("Code len: {}", code_len);

        println!("Starting pos: {}", self.pos);

        let pc = loop {
            let mut char_accumulator = format!(
                "pos: {}, rel-pos: {}, op: \"",
                self.pos,
                self.pos - sym_table_len - 1
            );

            let op_code = self.get_code(&mut char_accumulator);

            char_accumulator.push_str(format!("\" ({}) {{", op_code).as_str());

            if op_code == OP_RANGES[OP_IF as usize] {
                let (true_branch, new_stack) = Rib::pop_rib(&op_stack);
                op_stack = new_stack;

                op_repr_stack.push(OpRibRepr::If {
                    ribn_chars: char_accumulator.clone(),
                    true_branch: true_branch.clone(),
                    false_branch: op_stack.0.clone(),
                });

                op_stack.0 = Obj::rib(Rib(
                    Decoder::op_idx_to_rvm_op(OP_IF),
                    true_branch,
                    op_stack.0,
                ));
                continue;
            }

            let op_idx = (0..OP_RANGES.len() - 1)
                .rev()
                // find where next_code belongs by checking if it is greater
                // than the lower bound
                .find(|i| op_code >= OP_RANGES[*i])
                .unwrap();

            if op_idx as i32 == OP_JUMP {
                op_stack = Rib(Obj::Number(0), Obj::rib(op_stack), Obj::Number(0));
            }

            let mut op_value = op_code - OP_RANGES[op_idx];
            let short_op = SHORT_OP[op_idx];

            char_accumulator.push_str(
                format!("op-rel: {}, short-op: {} }}, arg: \"", op_value, short_op).as_str(),
            );

            let arg_type: &str;
            let mut next_value = if op_value == short_op {
                arg_type = "literal";
                op_value = self.get_int(0, &mut char_accumulator);
                Obj::Number(op_value)
            } else if op_value > short_op {
                arg_type = "index";
                op_value = self.get_int((op_value - short_op - 1) as i32, &mut char_accumulator);
                symbol_table.get(op_value as usize).clone()
            } else if op_idx < 3 {
                arg_type = "index";
                symbol_table.get(op_value as usize).clone()
            } else {
                arg_type = "literal";
                Obj::Number(op_value as i32)
            };
            char_accumulator
                .push_str(format!("\" ({}) {{ type: {} }}", op_value, arg_type).as_str());

            if op_idx == 5 {
                // closure const
                let (closure, new_stack) = Rib::pop_rib(&op_stack);
                op_stack = new_stack;
                let code_rib = Rib(next_value, Obj::rib(NIL), closure);
                let next_value_rib = Rib(Obj::rib(code_rib), Obj::rib(NIL), Obj::Number(1));
                if op_stack == NIL {
                    op_repr_stack.push(OpRibRepr::StartOfProgram {
                        ribn_chars: char_accumulator,
                    });
                    break next_value_rib;
                }
                next_value = Obj::rib(next_value_rib);
            }

            op_repr_stack.push(match op_idx {
                0 => OpRibRepr::Jump {
                    ribn_chars: char_accumulator,
                    slot_idx: op_value,
                },
                1 => OpRibRepr::Call {
                    ribn_chars: char_accumulator,
                    slot_idx: op_value,
                },
                2 => OpRibRepr::Set {
                    ribn_chars: char_accumulator,
                    slot_idx: op_value,
                },
                3 => OpRibRepr::Get {
                    ribn_chars: char_accumulator,
                    slot_idx: op_value,
                },
                4 => OpRibRepr::Const {
                    ribn_chars: char_accumulator,
                    object: next_value.clone(),
                },
                5 => OpRibRepr::ClosureConst {
                    ribn_chars: char_accumulator,
                    closure: next_value.clone(),
                },
                _ => unreachable!(),
            });

            op_stack.0 = Obj::rib(Rib(
                Decoder::op_idx_to_rvm_op(op_idx as i32),
                next_value,
                op_stack.0,
            ));
        };

        let Obj::Rib(pc) = pc.0 else { unreachable!() };
        let Obj::Rib(pc) = pc.2 else { unreachable!() };

        (
            *pc,
            OpRibRepr::vec_to_string(
                &op_repr_stack.into_iter().rev().collect::<Vec<OpRibRepr>>(),
                symbol_table,
                true,
            ),
        )
    }

    pub fn decode_symbol_table(&mut self) -> (SymbolTable, Vec<String>) {
        let mut decoded_vec = vec![String::new()];
        let mut symbol_table_rib = NIL;
        let mut first_char = String::new();
        let nb_empty = self.get_int(0, &mut first_char);
        for _ in 0..nb_empty {
            symbol_table_rib = Rib(
                Obj::rib(Rib(
                    Obj::Number(0),
                    Obj::rib(Rib(Obj::rib(NIL), Obj::Number(0), Obj::Number(3))),
                    Obj::Number(2),
                )),
                Obj::rib(symbol_table_rib),
                Obj::Number(0),
            )
        }

        let mut chars = NIL;
        let mut str_len = 0;
        let mut str_accumulator = String::new();
        loop {
            let next_char = self.get_byte(&mut str_accumulator);
            match next_char as u8 as char {
                ';' => break,
                ',' => {
                    symbol_table_rib = Rib(
                        Obj::rib(Rib(
                            Obj::Number(0),
                            Obj::rib(Rib(Obj::rib(chars), Obj::Number(str_len), Obj::Number(3))),
                            Obj::Number(2),
                        )),
                        Obj::rib(symbol_table_rib),
                        Obj::Number(0),
                    );
                    chars = NIL;
                    str_len = 0;
                    decoded_vec.push(String::new());
                }
                _ => {
                    decoded_vec
                        .last_mut()
                        .unwrap()
                        .insert(0, next_char as u8 as char);
                    chars = Rib(
                        Obj::Number(next_char as i32),
                        Obj::rib(chars),
                        Obj::Number(0),
                    );
                    str_len += 1;
                }
            }
        }
        symbol_table_rib = Rib(
            Obj::rib(Rib(
                Obj::Number(0),
                Obj::rib(Rib(Obj::rib(chars), Obj::Number(str_len), Obj::Number(3))),
                Obj::Number(2),
            )),
            Obj::rib(symbol_table_rib),
            Obj::Number(0),
        );

        (
            SymbolTable {
                symbols: symbol_table_rib,
            },
            decoded_vec,
        )
    }
}

impl ToString for Obj {
    fn to_string(&self) -> String {
        match self {
            Obj::Number(num) => format!("{num}"),
            Obj::Rib(rib) => data_rib_to_string(*rib.clone()),
        }
    }
}

pub fn pair_to_string(pair: Rib) -> String {
    let mut strings = String::new();
    let mut curr = pair.0.as_rib().unwrap();
    while curr != NIL {
        let Obj::Number(character) = curr.0 else {
            panic!("The rib is not a pair, got {:?}", curr);
        };

        strings.push(character as u8 as char);
        curr = curr.1.as_rib().unwrap();
    }

    strings
}

pub fn data_rib_to_string(data_rib: Rib) -> String {
    let Obj::Number(data_code) = data_rib.2 else {
        panic!("The rib is not a data_rib, got {:?}", data_rib);
    };

    match data_code {
        0 => format!(
            "{}, {}",
            data_rib.0.to_string(),
            op_rib_to_string(&data_rib.1.as_rib().unwrap())
        ), // pair
        1 => procedure_rib_to_string(&data_rib), // procedure
        2 => format!("'{}", {
            let symbol = pair_to_string(data_rib.1.as_rib().unwrap());
            if symbol.is_empty() {
                " ".to_owned()
            } else {
                symbol
            }
        },), // variable
        3 => format!("\"{}\"", pair_to_string(data_rib)), // string
        4 => "[...]".to_owned(),                 // vector
        5 => match data_rib {
            // special value
            NIL => "nil".to_owned(),
            _ => unreachable!(),
        },
        _ => panic!("Unknown data code {}", data_code),
    }
}

fn procedure_rib_to_string(procedure_rib: &Rib) -> String {
    let mut curr_op = procedure_rib.0.as_rib().unwrap();
    let env = procedure_rib.1.clone();

    let mut procedure_str = format!("procedure(env: {}) {{", env.to_string(),);

    while curr_op != NIL {
        procedure_str.push_str(format!("\n\t{}", op_rib_to_string(&curr_op).as_str()).as_str());
        if let Obj::Rib(next) = curr_op.2.clone() {
            curr_op = *next;
        } else {
            break;
        }
    }

    procedure_str.push_str("\n}");

    procedure_str
}

pub fn op_rib_to_string(op_rib: &Rib) -> String {
    let Obj::Number(op_code) = op_rib.0 else {
        panic!("The rib is not an op_rib");
    };

    if op_code == OP_IF - 1 {
        return "if true_branch false_branch".to_owned();
    }

    let arg = match &op_rib.1 {
        Obj::Number(num) => format!("{num}"),
        Obj::Rib(rib) => data_rib_to_string(*rib.clone()),
    };

    let op_to_string = String::from(match op_code {
        0 => match op_rib.2 {
            Obj::Number(_) => format!("(jump) {{\n\tx ← call {arg};\n\tjump;\n\tpush x;\n}}"),
            Obj::Rib(_) => format!("push (call {arg})"),
        },
        1 => format!("(set) {arg} ← pop()"),
        2 => format!("(get) push {arg}"),
        3 => format!("(const) push {arg}"), // const
        9 => format!("(closure const) push {arg}"),
        _ => panic!("Unknown op code {}", op_code),
    });

    op_to_string
}

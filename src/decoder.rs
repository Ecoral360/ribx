use std::{
    ops::{Range, RangeInclusive},
    rc::Rc,
};

struct SymbolTable {
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

#[derive(Debug, Clone)]
struct Rib(Obj, Obj, Obj);

#[derive(Debug, Clone)]
enum Obj {
    Number(i32),
    Rib(Box<Rib>),
}

const SHORT_OP: [u8; 6] = [20u8, 30u8, 0u8, 10u8, 11u8, 4u8];
const OP_JUMP: u8 = 22;
const OP_CALL: u8 = 55;
const OP_SET: u8 = 58;
const OP_GET: u8 = 71;
const OP_CONST: u8 = 85;
const OP_CLOSURE_CONST: u8 = 90;
const OP_IF: u8 = 91;
const OP_RANGES: [u8; 7] = [
    OP_JUMP,
    OP_CALL,
    OP_SET,
    OP_GET,
    OP_CONST,
    OP_CLOSURE_CONST,
    OP_IF,
];

enum Data {
    Pair {
        /// Current element
        car: Rc<Data>,
        /// Rest of the linked list
        cdr: Rc<Data>,
    },
    Procedure {
        code: Obj,
        /// Can be a Pair or NIL
        env: Rc<Data>,
    },
    Symbol {
        value: Box<Data>,
    },
    String {},
    Vector {},
    Constant,
}

enum Operation {
    Jump {
        /// The procedure to call (the first field of the rib is either a int (primitive) or a rib
        /// (non-primitive)
        procedure: Rib,
    },
    Call {
        /// The procedure to call (the first field of the rib is either a int (primitive) or a rib
        /// (non-primitive)
        procedure: Rib,
        // next: ,
    },
    Set {
        /// A rib containing a symbol which represents the variable to set.
        /// Value ← pop()
        variable: Rib,
        // next: ,
    },
    Get {
        variable: Rib, // next: ,
    },
    Const {},
    /// If pop() #t -> goto first, else goto second
    If {
        
    },
}

impl Operation {
    fn get_code(&self) -> u8 {
        match &self {
            Operation::Jump { .. } | Operation::Call { .. } => 0,
            Operation::Set { .. } => 1,
            Operation::Get { .. } => 2,
            Operation::Const { .. } => 3,
            Operation::If { .. } => 4,
        }
    }

    fn as_rib(&self) -> Rib {
        todo!()
    }
}

struct Decoder {
    ribn: String,
    pos: usize,
}

impl Decoder {
    fn new(ribn: &str) -> Decoder {
        Decoder {
            ribn: ribn.to_owned(),
            pos: 0,
        }
    }

    fn get_code(&mut self, accumulator: &mut String) -> u8 {
        let code = (&self.ribn.chars().nth(self.pos)).unwrap();
        self.pos += 1;
        accumulator.push(code);
        code as u8
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

    fn decode_op(&mut self, op_stack: &mut Rib, symbol_table: &mut SymbolTable) -> (Operation, String) {
        let mut char_accumulator = String::new();
        let next_code = self.get_code(&mut char_accumulator);
        if next_code == OP_IF {

            return (Operation::If {}, char_accumulator);
        }

        // find the  

        todo!()
    }

    pub fn decode(&mut self, symbol_table: &mut SymbolTable) -> String {
        let pc: Obj;
        loop {
            break;
        }

        todo!()
    }
}

fn decode_symbol_table() {}

fn abc() {}

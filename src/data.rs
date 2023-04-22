pub const NIL: Rib = Rib(Obj::Number(0), Obj::Number(0), Obj::Number(5));

pub enum Op {
    Jump,
    Call,
    Set,
    Get,
    Const,
    ClosureConst,
    If,
}

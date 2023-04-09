# RIBN

### Symbol table
`#epyt-gnirts,0dleif,epyt-erudecorp,epyt-lobmys,epyt-rotcev,x,1dleif,epyt-riap,gnirts>-lobmys,esolc,?bir,*,!tes-1dleif,2dleif,?llun,?rotcev,?erudecorp,gnirts>-rebmun,tneitouq,tsil>-rotcev,?lobmys,-,gnirts>-tsil,?gnirts,ton,snoc,htgnel,pool,2gra,tsil-etirw,tsil>-gnirts,xua-gnirts>-rebmun,<,rac,etirw,2rahctup,+,yalpsid,?ecnatsni,?riap,di,?vqe,srahc-etirw,rdc,rahctup,1gra,,,,bir;`


1. First char: number of symbols with an empty name.
2. Until the last char: list of the reversed name of the procedures separated by `,`.
3. Last char: `;`.

> Notes:
> 1. The functions in the symbol table are in order of use
> 2. A function name is *usually* only kept in the symbol table if the function name is used
> as a value or the function is exported using the `(export <function>)` function.

```
89k!9,i$89Gl^>?u>FiO!OGliO~Ku^z!Ok!1(_>?^{!*,i$*a@_>?^*a@_>?^>?vS#~OBvS#_*a@_>?^*a@_>?^>?vS#~O^~^BvE^*a@_>?vS;>?vS#~Bt^*a@_>?vS9>?vS#~Bv0^*a@_>?vS5>?vS#~Bu^*a@_>?^~S`J^~D^{!7,i$,i$87@^>IJ^~D^>?vC~D^z!//YC^1vS7vF~YD^2YA^>?vF~YE^*i$M^~T^/YL^~W^(vL>N@^>IJ^>?vK~D^1vLvK~YF^1vS;vF~Bi%^1vS-vF~S^z!2/^(vE>Ai%M^>?vE~T^z!DEl!Aj%!EEo!6j%!>#nQ_^z!=En!5,^5_`~KakRb^GVYIu``vR%YBu^{!C8>Li&^8>RLi&V`kvP~Kk^z>YHki#!LiN!@Em!:,k0Q@_l~D^z!F+i&^z!)iN!3j%!;#k`^{!-Ek!<+i$^z!.8K,i$+bYG^~YJ^zz!Po]&n]#m]$l!Mk!(:lkl!B:lkm!I:lkn!?:lko!0:lkp!4:lkq!+:lkr!H:lks!G:lkt!N:lku]%:lkv.!J:lkv/!K:lkv0!8:lkv1!':lkv2!,:lkv3y
```


### Decoding the RIBN 

First, we need to define 2 utility functions that will help us when decoding the 
RIBN: `getCode()` and `getInt()`.
```
getCode()
    next = read next RIBN character as byte
    if next < 35:
        return 57  // default value
    else:
        return next - 35  // adjusted value


getInt(totalBase46: int)
    /*
        Ints are encoded in base 46. To decode, we need to know the 
        total number of base 46 digits.
    */

    // Get the next code:
    nextInt = getCode()

    // Multiply the total by 46. 
    // This has the effect of shifting the digits to the left by one place:
    totalBase46 *= 46 
    
    // To signal that the decoding is not yet complete, 
    // the int was offsetted by 46
    if nextInt >= 46:
        // we get the real value of the int, 
        // as it was only >= 46 to signal another digit
        nextInt -= 46
        return getInt(nextInt + totalBase46)
    else:
        return nextInt + totalBase46
```

Now, we can get to the decoding part!

```
op ranges: [
    0..=22  (jump),
    23..=55 (call),
    56..=58 (set), 
    59..=71 (get), 
    72..=85 (const),
    86..=90 (closure const),
    91      (if),
]

shortOp = [
    20 (jump),
    30 (call),
    0 (set),
    10 (get),
    11 (const),
    4 (closure const),
]

Note: shortOps are always 2 less than the end of their associated op range.
      Ex: shortOp[jump] + 2 == op[jump].end

decode()
    set opStack to NIL
    define nextStackValue
    loop:
        nextStackValue = null

        nextCode = getCode()

        opIdx = index of op where nextCode in op range
        opName = name of op at opIdx
        opValue = relative value of the code to fit inside their op range
        Note: opValue is never > shortOp[opIdx] + 2 by definition

        if opName is 'if': // (nextCode == 91)
            // we pop the opStack because the true branch is on top of the opStack
            // when we get here
            nextStackValue = opStack.pop()
            // 4 is the op code for 'if' in the rvm.
            opStack = rib(4, nextStackValue, opStack.top())
            continue
        
        nextStackValue = when
            // this means opValue is a literal int value encoded 
            // as a multi-byte int base 46
            opValue == shortOp[opIdx] -> getInt(0)

            // this means opValue is an index encoded as a multi-byte int
            // base 46 where the first byte contributes a 0 or 1 as the first
            // digit of the int base 46
            opValue > shortOp[opIdx] ->
                valueIdx = getInt(opValue - d - 1)
                getSymbolFromRef(valueIdx) // gets a symbol from the symbol table

            // this means opValue is the literal index of a symbol
            // get the symbol from the symbol table
            // (opIdx < 3)
            opName is 'jump', 'call', or 'set' -> getSymbolFromRef(opValue)

            // this means opValue is the literal int value
            else -> opValue

        if opName is 'closure const': // (opIdx == 5)
            // the top of the opStack contains the continuation of the program
            codeRib = rib(opValue, 0, opStack.pop())

            // rib procedures follow the form: (code, env, 1)
            nextStackValue = rib(codeRib, NIL, 1) // 1 means this is a procedure

            if opStack is empty:
                break
            // we put opIdx to 4 to make it have the same code as the 'const' operation
            opIdx = 4
        
        // we subtract 1 from opIdx to transform the code to it's rvm op code:
        // 0 = call/jump, 1 = set, 2 = get, 3 = const, 4 = if.
        opStack = rib(max(opIdx - 1, 0), nextStackValue, opStack.top())
    
    // at this point, nextStackValue is a rib of the form:
    // (codeRib, NIL, 1) 
    //      where codeRib is a rib of the form:
    //          (someValue, 0, continuation)
    // this is certain because the loop can only exit when the op is 'closure const'

    // the program counter (pc) starts at the first continuation, so:
    codeRib = nextStackValue[0]
    pc = codeRib[2]
```


The `merge` op proposal:
```

```




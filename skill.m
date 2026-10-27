% SKI language v2 with additional B and C combinators.
% B x y z = x (y z)
% C x y z = x z y
% def B : S (K S) K
% def C : S (S (K (S (K S) K)) S) (K K)

clear all;
close all;

% control for the interpreter whether arguments should be evaluated
global eager = false; % opposite lazy
global minparen = true; % opposite maxparen
global debugon = false; % opposite debugoff

lex.EOF = 0;
lex.S = 1;
lex.K = 2;
lex.I = 3;
lex.B = 4;
lex.C = 5;
lex.LPAREN = 6;
lex.RPAREN = 7;
lex.COLON = 8;
lex.DEFINE = 9;
lex.ID = 10;
lex.NULL = 11;

function str = symbol2string(symbol, def)
    s = symbol;
    d = def;
    if (s == d.NULL)
        str = "NULL";
    elseif (s == d.S)
        str = "S";
    elseif (s == d.K)
        str = "K"; 
    elseif (s == d.I)
        str = "I";
    elseif (s == d.B)
        str = "B";
    elseif (s == d.C)
        str = "C";
    elseif (s == d.LPAREN)
        str = "(";
    elseif (s == d.RPAREN)
        str = ")";
    elseif (s == d.RPAREN)
        str = ")";
    elseif (s == d.COLON)
        str = ":";
    elseif (s == d.DEFINE)
        str = "def";
    elseif (s == d.EOF)
        str = "EOF";
    else
        str = "";
    end
end

function sym = getKeyword(s,def)
    if (strcmp(s,"def"))
        sym = def.DEFINE;
    else
        sym = def.ID;
    end
end

function [sym text] = get(text, def)
    while((text.pos <= text.len) && isspace(text.ch))
        text = textAdv(text,1);
    end
    dpos = 1;
    ch = text.ch;
    if (isempty(ch))
        sym = def.EOF;
    elseif (ch == 'S')
        sym = def.S;
    elseif (ch == 'K')
        sym = def.K;
    elseif (ch == 'I')
        sym = def.I;
    elseif (ch == 'B')
        sym = def.B;
    elseif (ch == 'C')
        sym = def.C;
    elseif (ch == '(')
        sym = def.LPAREN;
    elseif (ch == ')')
        sym = def.RPAREN;
    elseif (ch == ':')
        sym = def.COLON;
    elseif (isletter(text.ch))
        start = text.pos;
        while (isletter(text.ch) || isasciinum(text.ch))
            text = textAdv(text,1);
        end
        stop = text.pos-1;
        text.id = text.source(start:stop);
        sym = getKeyword(text.id, def);
        dpos = 0;
    else
        sym = def.NULL;
    end
    text = textAdv(text,dpos);
end

function text = init(source);
    text.source = source;
    text.len = length(text.source);
    text.pos = 1;
    if (text.len > 0)
        text.ch = text.source(text.pos);
    else
        text.ch = "";
    end
    text.id = "";
    text.val = 0;
    text.erlog = "";
end

% TEXT ADV (TEXT ADVANCE)
function t = textAdv(text, amount)
    text.pos = text.pos + amount;
    if (text.pos <= text.len)
        text.ch = text.source(text.pos);
    else
        text.ch = "";
    end
    t = text;
end

% S = E|D
% D = def id : E
% E = A|F  
% E'= F E'|
% A = F F E'|F (A)| 
% F = s|k|i|b|c|id

function [b text p] = parse(text, def)
    [sym t1] = get(text,def);
    if (sym == def.DEFINE)
        [b text p] = definition(text, def);
    else
        [b text p] = funapp(text, def);
    end 
end

function [b text p] = definition(text, def)
    [sym text] = get(text, def);
    if (sym == def.DEFINE)
        [sym text] = get(text, def);
        if (sym == def.ID)
            f.val = text.id;
            f.type = def.ID;
            [sym text] = get(text, def);
            if (sym == def.COLON)
                [b text a] = funapp(text, def);
                if (b == true)
                    p.left = f;
                    p.right = a;
                    p.type = def.DEFINE;
                end
            else
                parseErr("expected ':'",text);
            end
        else
            parseErr("expected ID",text);
        end
    else
        b = false;
        p.type = def.EOF;
    end
end

function [b text p] = funapp(text, def)
    [b1 t1 p1] = app(text, def);
    [b2 t2 p2] = fun(text, def);
    if (b1 == true)
        b = b1;
        text = t1;
        p = p1;
        [sym text] = get(text, def);
        if (sym ~= def.EOF)
            b = false;
        end
    elseif (b2 == true)
        b = b2;
        text = t2;
        p = p2;
        [sym text] = get(text, def);
        if (sym ~= def.EOF)
            b = false;
        end
    else
        b = false;
        text = t2;
        p = p2;
    end
end

function [b text p] = funappdash(text, def, c)
    [sym t1] = get(text, def);
    if (sym == def.EOF)
        b = true;
        text = t1;
        p = c;
    elseif (sym == def.RPAREN) % another kind of epsilon
        b = true;
        p = c;
    elseif (sym == def.LPAREN)
        text = t1;
        [b text a] = app(text, def);
        [sym text] = get(text, def);
        if (sym == def.RPAREN)
            p.type = def.NULL;
            p.left = c;
            p.right = a;
            [b text p] = funappdash(text, def, p);
        else
            p.type = def.EOF;
            b = false;
        end
    else
        [b text a] = fun(text, def);
        if (b == true)
            p.type = def.NULL;
            p.left = c;
            p.right = a;
            [b text p] = funappdash(text, def, p);
        else
            b = false;
            p.type = def.EOF;
        end
    end
end

function [b text p] = app(text, def)
    [b text f] = fun(text, def);
    if (b == true)
        [sym t1] = get(text, def);
        if (sym == def.LPAREN)
            text = t1;
            [b text a] = app(text, def);
            [sym text] = get(text, def);
            if (sym ~= def.RPAREN)
                p.type = def.EOF;
                b = false;
                parseErr("expected ')'",text);
            else
                c.type  = def.NULL;
                c.left  = f;
                c.right = a;
                [b text p] = funappdash(text, def, c);
            end
        else
            [b text a] = fun(text, def);
            if (b == true)
                c.type  = def.NULL;
                c.left  = f;
                c.right = a;
                [b text p] = funappdash(text, def, c);
            else
                p.type = def.EOF;
            end
        end
    else
        p1.type = def.EOF;
        p = p1;
    end
end

function [b text p] = fun(text, def)
    [sym, text] = get(text, def);
    if (sym == def.S)
        b = true;
        p.type = def.S;
    elseif (sym == def.K)
        b = true;
        p.type = def.K;
    elseif (sym == def.I)
        b = true;
        p.type = def.I;
    elseif (sym == def.B)
        b = true;
        p.type = def.B;
    elseif (sym == def.C)
        b = true;
        p.type = def.C;
    elseif (sym == def.ID)
        b = true;
        p.type = def.ID;
        p.val = text.id;
    else
        b = false;
        p.type = def.EOF;
    end
end

function str = parseErr(message, text)
    display(sprintf("Error: %s\n",message));
    display(sprintf(text.source));
    display([blanks(text.pos-1) '^']);
end

function str = tree2str(p, def)
    global minparen;
    if (p.type == def.NULL)
        f = tree2str(p.left, def);
        a = tree2str(p.right, def);
        if (minparen)
            if (p.right.type == def.NULL)
                str = [f " " '(' a ')'];
            else
                str = [f " " a];
            end
        else
            str = ['(' f " " a ')'];
        end
    elseif (iscomb(p.type, def))
        str = symbol2string(p.type, def);
    elseif (p.type == def.DEFINE)
        a = tree2str(p.right, def);
        str = ["def " p.left.val " : " a];
    elseif (p.type == def.ID)
        str = p.val;
    end
end

function code = generateCode(p,def)
    
end


function bind = newBind(bound, name, value)
    bind = bound;
    if (~isempty(bind))
        for n = 1:length(bind)
            if (strcmp(name,bound(n).name))
                bind(n).value = value;
                return;
            end
        end
        d.name = name;
        d.value = value;
        bind = [bind d];
    else
        b1.name = name;
        b1.value = value;
        bind = b1;
    end
end

function [p defined] = lookupBind(bound, name)
    if (~isempty(bound))
        if (length(bound) == 1)
            if strcmp(name, bound.name)
                p = bound.value;
                defined = true;
                return;
            end
        end
        for bind = bound
            if (strcmp(name,bind.name))
                p = bind.value;
                defined = true;
                return;
            end
        end
    end
    p.type = 0;
    defined = false;
end

function b =iscomb(f, def)
    b = f==def.S||f==def.K||f==def.I||f==def.B||f==def.C;
end

% check whether the function being applied is actually a constant
% this is the leftmost part of the tree
function b = isconst(p, def, bind)
    if (p.type == def.ID)
        [throwaway,b] = lookupBind(bind,p.val);
        b = ~b;
    else
        while (p.type == def.NULL)
            p = p.left;
        end
        if (iscomb(p.type,def)||p.type==def.DEFINE)
            b = 0;
        elseif (p.type == def.ID)
            [throwaway,b] = lookupBind(bind,p.val); 
            b = ~b;
        else
            b = true;
        end
    end
end

function [r d] = runSKI(p,def,bind)
    global eager;
    global debugon;
    d = bind;
    if (debugon)
        display(sprintf("INTERPRETING: %s", tree2str(p,def))) % DEBUG
    end
    if (isconst(p, def, bind) && ~eager)
        % This was added to allow many undefined functions
        r = p;
    elseif (p.type == def.NULL)
        if (eager) % evaluate argument if possible (eager evaluation)
            [p.right d] = runSKI(p.right,def,bind);
        end
        l1 = p.left;
        if (l1.type == def.I)
            [r d] = runSKI(p.right,def,bind);
        elseif (l1.type == def.ID)
            [p1 b] = lookupBind(bind,l1.val);
            if (b == true)
                p.left = p1;
                [r d] = runSKI(p,def,bind);
            else
                d = bind;
                r = p;
            end
        elseif (l1.type == def.NULL)
            l2 = l1.left;
            if (l2.type == def.K)
                [r d] = runSKI(l1.right,def,bind);
            elseif (l2.type == def.I || l2.type == def.ID)
                if (l2.type == def.ID)
                    [p1 b] = lookupBind(bind,l2.val);
                    if (b == true)
                        p.left.left = p1;
                        [r d] = runSKI(p,def,bind);
                    else
                        d = bind;
                        r = p;
                    end
                else
                    [p.left bind] = runSKI(l1,def,bind);
                    [r d] = runSKI(p,def,bind);
                end
            elseif (l2.type == def.NULL)
                l3 = l2.left;
                if (l3.type == def.S)
                    z = p.right;
                    y = l1.right;
                    x = l2.right;
                    yz.left = y;
                    yz.right = z;
                    yz.type = def.NULL;
                    xz.left = x;
                    xz.right = z;
                    xz.type = def.NULL;
                    r1.left = xz;
                    r1.right = yz;
                    r1.type = def.NULL;
                    [r d] = runSKI(r1,def,bind);
                elseif (l3.type == def.B)
                    z = p.right;
                    y = l1.right;
                    x = l2.right;
                    yz.left = y;
                    yz.right = z;
                    yz.type = def.NULL;
                    r1.left = x;
                    r1.right = yz;
                    r1.type = def.NULL;
                    [r d] = runSKI(r1,def,bind);
                elseif (l3.type == def.C)
                    z = p.right;
                    y = l1.right;
                    x = l2.right;
                    xz.left = x;
                    xz.right = z;
                    xz.type = def.NULL;
                    r1.left = xz;
                    r1.right = y;
                    r1.type = def.NULL;
                    [r d] = runSKI(r1,def,bind);
                elseif (l3.type==def.K||l3.type==def.I||l3.type==def.ID||l3.type == def.NULL)
                    if (l3.type == def.ID)
                        [p1 b] = lookupBind(bind,l3.val);
                        if (b == true)
                            p.left.left.left = p1;
                            [r d] = runSKI(p,def,bind);
                        else
                            d = bind;
                            r = p;
                        end
                    else
                        [p.left bind] = runSKI(l1,def,bind);
                        r = p;
                        d = bind;
                        [r d] = runSKI(p,def,bind);
                    end
                else
                    r = p;
                    d = bind;
                end
            else
                r = p;
                d = bind;
            end
        else
            r = p;
            d = bind;
        end
    elseif (iscomb(p.type,def))
        r = p;
        d = bind;
    elseif (p.type == def.ID)
        d = bind;
        p.val;
        [r b] = lookupBind(bind,p.val);
        if (b == false)
            r = p;
        end
    elseif (p.type == def.DEFINE)
        r = p;
        d = newBind(bind,p.left.val,p.right);
    else
        r = p;
        d = bind;
        display("Runtime error!");
    end     
end

function [d] = initBind(def)
    d = [];
    [tw1 tw2 bdif] = parse(init("I"), def);
    d = newBind(d, "if", bdif);
    
    [tw1 tw2 bdtrue] = parse(init("K"), def);
    d = newBind(d, "true", bdtrue);
    
    [tw1 tw2 bdfalse] = parse(init("K I"), def);
    d = newBind(d, "false", bdfalse);
    
    [tw1 tw2 bdswap] = parse(init("C I"), def);
    d = newBind(d, "swap", bdswap);
    
    [tw1 tw2 bdsucc] = parse(init("S B"), def);
    d = newBind(d, "succ", bdsucc);
    
    [tw1 tw2 bdplus] = parse(init("S B"), def);
    d = newBind(d, "plus", bdplus);
    
    [tw1 tw2 bdzero] = parse(init("K I"), def);
    d = newBind(d, "zero", bdzero);
    [tw1 tw2 bdone] = parse(init("I"), def);
    d = newBind(d, "one", bdone);
    [tw1 tw2 bdtwo] = parse(init("succ one"), def);
    d = newBind(d, "two", bdtwo);
    [tw1 tw2 bdthree] = parse(init("succ two"), def);
    d = newBind(d, "three", bdthree);
    [tw1 tw2 bdfour] = parse(init("succ three"), def);
    d = newBind(d, "four", bdfour);
    [tw1 tw2 bdfive] = parse(init("succ four"), def);
    d = newBind(d, "five", bdfive);
    [tw1 tw2 bdsix] = parse(init("succ five"), def);
    d = newBind(d, "six", bdsix);
    [tw1 tw2 bdseven] = parse(init("succ six"), def);
    d = newBind(d, "seven", bdseven);
    [tw1 tw2 bdeight] = parse(init("succ seven"), def);
    d = newBind(d, "eight", bdeight);
    [tw1 tw2 bdnine] = parse(init("succ eight"), def);
    d = newBind(d, "nine", bdnine);
    
    [tw1 tw2 bdadd] = parse(init("swap plus"), def);
    d = newBind(d, "add", bdadd);
    
    [tw1 tw2 bdmult] = parse(init("B"), def);
    d = newBind(d, "mult", bdmult);
    
    [tw1 tw2 bdpow] = parse(init("swap"), def);
    d = newBind(d, "pow", bdpow);
    
    [tw1 tw2 bdpair] = parse(init("B C swap"), def);
    d = newBind(d, "pair", bdpair); 
    
    [tw1 tw2 bdfirst] = parse(init("swap true"), def);
    d = newBind(d, "first", bdfirst);
    
    [tw1 tw2 bdsecond] = parse(init("B first rest"), def);
    d = newBind(d, "second", bdsecond);
    
    [tw1 tw2 bdrest] = parse(init("swap false"), def);
    d = newBind(d, "rest", bdrest);
    
    [tw1 tw2 bdnil] = parse(init("K true"), def);
    d = newBind(d, "nil", bdnil);
    
    [tw1 tw2 bdisempty] = parse(init("swap (K (K false))"), def);
    d = newBind(d, "isempty", bdisempty);
    
    [tw1 tw2 bdfix] = parse(init("S (C B (S I I)) (C B (S I I))"), def);
    d = newBind(d, "fix", bdfix);

    [tw1 tw2 bdnumtolist] = parse(init("C (swap listsucc) nil"), def);
    d = newBind(d, "numtolist", bdnumtolist);

    [tw1 tw2 bdlisttonumbod] = parse(init("B (S (C isempty zero)) (B (S (B add first)) (C B rest))"), def);
    d = newBind(d, "listtonumbod", bdlisttonumbod);

    [tw1 tw2 bdlisttonum] = parse(init("fix listtonumbod"), def);
    d = newBind(d, "listtonum", bdlisttonum);
    
    [tw1 tw2 bdlistpred] = parse(init("rest"), def);
    d = newBind(d, "listpred", bdlistpred);
    
    [tw1 tw2 bdlistpred] = parse(init("pair one"), def);
    d = newBind(d, "listsucc", bdlistpred);

    [tw1 tw2 bdphi] = parse(init("S (B pair second) (B succ second)"), def);
    d = newBind(d, "phi", bdphi);
    
    [tw1 tw2 bdpred] = parse(init("S (K first) (pair phi (pair zero zero))"), def);
    d = newBind(d, "pred", bdpred);
    
    [tw1 tw2 bdsub] = parse(init("B C swap pred"), def);
    d = newBind(d, "sub", bdsub);
    
    [tw1 tw2 bdiszero] = parse(init("pair (K false) true"), def);
    d = newBind(d, "iszero", bdiszero);
end

bind = initBind(lex);
src = input('>> ','s');
while(~strcmp(src,"end")) 
    if (strcmp(src,"lazy"))
        eager = false % remember that this is a global
    elseif (strcmp(src,"eager"))
        eager = true % remember that this is a global
    elseif (strcmp(src,"maxparen"))
        minparen = false % remember that this is a global
    elseif (strcmp(src,"minparen"))
        minparen = true % remember that this is a global
    elseif (strcmp(src,"debugon"))
        debugon = true % remember that this is a global
    elseif (strcmp(src,"debugoff"))
        debugon = false % remember that this is a global
    else
        text = init(src);
        [b text p] = parse(text, lex);
        b
        if (b)
            tree2str(p,lex)
            [r bind] = runSKI(p,lex,bind);
            tree2str(r,lex)
        end
    end
    src = input('>> ','s');
end

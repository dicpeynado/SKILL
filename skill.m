% SKI language with bindings
clear all;
close all;

lex.EOF = 0;
lex.S = 1;
lex.K = 2;
lex.I = 3;
lex.LPAREN = 4;
lex.RPAREN = 5;
lex.COLON = 6;
lex.DEFINE = 7;
lex.ID = 8;
lex.NULL = 9;

function str = symbol2string(symbol, defs)
    s = symbol;
    d = defs;
    if (s == d.NULL)
        str = "NULL";
    elseif (s == d.S)
        str = "S";
    elseif (s == d.K)
        str = "K"; 
    elseif (s == d.I)
        str = "I";
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
% F = s|k|i|id

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
    if (p.type == def.NULL)
        f = tree2str(p.left, def);
        a = tree2str(p.right, def);
        str = ['(' f " " a ')'];
    elseif (p.type == def.S)
        str = 'S';
    elseif (p.type == def.K)
        str = 'K';
    elseif (p.type == def.I)
        str = 'I';
    elseif (p.type == def.DEFINE)
        a = tree2str(p.right, def);
        str = ["def " p.left.val " : " a];
    elseif (p.type == def.ID)
        str = p.val;
    end
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

function b = isconst(p, def, bind)
    if (p.type == def.ID)
        [throwaway,b] = lookupBind(bind,p.val);
        b = ~b;
    else
        while (p.type == def.NULL)
            p = p.left;
        end
        if (p.type==def.S||p.type==def.K||p.type==def.I||p.type==def.DEFINE)
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
    d = bind;
    sprintf("INTERPRETING: %s", tree2str(p,def)) % DEBUG
    if (isconst(p, def, bind))
        % This was added to allow many undefined functions
        r = p;
    elseif (p.type == def.NULL)
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
    elseif (p.type == def.S || p.type == def.K || p.type == def.I)
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
    [tw1 tw2 bdatobc] = parse(init("S (K S) K"), def);
    d = newBind(d, "atobc", bdatobc);
    [tw1 tw2 bdswap] = parse(init("S (K (S I)) K"), def);
    d = newBind(d, "swap", bdswap);
    [tw1 tw2 bdsucc] = parse(init("S atobc"), def);
    d = newBind(d, "succ", bdsucc);
    [tw1 tw2 bdzero] = parse(init("K I"), def);
    d = newBind(d, "zero", bdzero);
    [tw1 tw2 bdone] = parse(init("I"), def);
    d = newBind(d, "one", bdone);
    % abdtocd = S (K S)
    % abd = S (K K)
end

bind = initBind(lex);
src = input('>> ','s');
while(~strcmp(src,"end")) 
    text = init(src);
    [b text p] = parse(text, lex);
    b
    if (b)
        tree2str(p,lex)
        [r bind] = runSKI(p,lex,bind);
        tree2str(r,lex)
    end
    src = input('>> ','s');
end
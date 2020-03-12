import "../lib/github.com/diku-dk/lys/lys"

type input_states =
    {
        a: i8,
        b: i8,
        c: i8,
        d: i8,
        e: i8,
        f: i8,
        g: i8,
        h: i8,
        i: i8,
        j: i8,
        k: i8,
        l: i8,
        m: i8,
        n: i8,
        o: i8,
        p: i8,
        q: i8,
        r: i8,
        s: i8,
        t: i8,
        u: i8,
        v: i8,
        w: i8,
        x: i8,
        y: i8,
        z: i8,
        arrowup: i8,
        arrowleft: i8,
        arrowdown: i8,
        arrowright: i8,
        1: i8,
        2: i8,
        3: i8,
        4: i8,
        5: i8,
        6: i8,
        7: i8,
        8: i8,
        9: i8,
        0: i8
    }

let keydown key (inputs: input_states) =
        if key == SDLK_a then inputs with a = 1
        else if key == SDLK_b then inputs with b = 1
        else if key == SDLK_c then inputs with c = 1
        else if key == SDLK_d then inputs with d = 1
        else if key == SDLK_e then inputs with e = 1
        else if key == SDLK_f then inputs with f = 1
        else if key == SDLK_g then inputs with g = 1
        else if key == SDLK_h then inputs with h = 1
        else if key == SDLK_i then inputs with i = 1
        else if key == SDLK_j then inputs with j = 1
        else if key == SDLK_k then inputs with k = 1
        else if key == SDLK_l then inputs with l = 1
        else if key == SDLK_m then inputs with m = 1
        else if key == SDLK_n then inputs with n = 1
        else if key == SDLK_o then inputs with o = 1
        else if key == SDLK_p then inputs with p = 1
        else if key == SDLK_q then inputs with q = 1
        else if key == SDLK_r then inputs with r = 1
        else if key == SDLK_s then inputs with s = 1
        else if key == SDLK_t then inputs with t = 1
        else if key == SDLK_u then inputs with u = 1
        else if key == SDLK_v then inputs with v = 1
        else if key == SDLK_w then inputs with w = 1
        else if key == SDLK_x then inputs with x = 1
        else if key == SDLK_y then inputs with y = 1
        else if key == SDLK_z then inputs with z = 1
        else if key == SDLK_UP then inputs with arrowup = 1
        else if key == SDLK_LEFT then inputs with arrowleft = 1
        else if key == SDLK_DOWN then inputs with arrowdown = 1
        else if key == SDLK_RIGHT then inputs with arrowright = 1
        else if key == SDLK_1 then inputs with 1 = 1
        else if key == SDLK_2 then inputs with 2 = 1
        else if key == SDLK_3 then inputs with 3 = 1
        else if key == SDLK_4 then inputs with 4 = 1
        else if key == SDLK_5 then inputs with 5 = 1
        else if key == SDLK_6 then inputs with 6 = 1
        else if key == SDLK_7 then inputs with 7 = 1
        else if key == SDLK_8 then inputs with 8 = 1
        else if key == SDLK_9 then inputs with 9 = 1
        else if key == SDLK_0 then inputs with 0 = 1
        else inputs

let keyup key (inputs: input_states) =
        if key == SDLK_a then inputs with a = 0
        else if key == SDLK_b then inputs with b = 0
        else if key == SDLK_c then inputs with c = 0
        else if key == SDLK_d then inputs with d = 0
        else if key == SDLK_e then inputs with e = 0
        else if key == SDLK_f then inputs with f = 0
        else if key == SDLK_g then inputs with g = 0
        else if key == SDLK_h then inputs with h = 0
        else if key == SDLK_i then inputs with i = 0
        else if key == SDLK_j then inputs with j = 0
        else if key == SDLK_k then inputs with k = 0
        else if key == SDLK_l then inputs with l = 0
        else if key == SDLK_m then inputs with m = 0
        else if key == SDLK_n then inputs with n = 0
        else if key == SDLK_o then inputs with o = 0
        else if key == SDLK_p then inputs with p = 0
        else if key == SDLK_q then inputs with q = 0
        else if key == SDLK_r then inputs with r = 0
        else if key == SDLK_s then inputs with s = 0
        else if key == SDLK_t then inputs with t = 0
        else if key == SDLK_u then inputs with u = 0
        else if key == SDLK_v then inputs with v = 0
        else if key == SDLK_w then inputs with w = 0
        else if key == SDLK_x then inputs with x = 0
        else if key == SDLK_y then inputs with y = 0
        else if key == SDLK_z then inputs with z = 0
        else if key == SDLK_UP then inputs with arrowup = 0
        else if key == SDLK_LEFT then inputs with arrowleft = 0
        else if key == SDLK_DOWN then inputs with arrowdown = 0
        else if key == SDLK_RIGHT then inputs with arrowright = 0
        else if key == SDLK_1 then inputs with 1 = 0
        else if key == SDLK_2 then inputs with 2 = 0
        else if key == SDLK_3 then inputs with 3 = 0
        else if key == SDLK_4 then inputs with 4 = 0
        else if key == SDLK_5 then inputs with 5 = 0
        else if key == SDLK_6 then inputs with 6 = 0
        else if key == SDLK_7 then inputs with 7 = 0
        else if key == SDLK_8 then inputs with 8 = 0
        else if key == SDLK_9 then inputs with 9 = 0
        else if key == SDLK_0 then inputs with 0 = 0
        else inputs

let init : input_states = 
    {
        a = 0,
        b = 0,
        c = 0,
        d = 0,
        e = 0,
        f = 0,
        g = 0,
        h = 0,
        i = 0,
        j = 0,
        k = 0,
        l = 0,
        m = 0,
        n = 0,
        o = 0,
        p = 0,
        q = 0,
        r = 0,
        s = 0,
        t = 0,
        u = 0,
        v = 0,
        w = 0,
        x = 0,
        y = 0,
        z = 0,
        arrowup = 0,
        arrowleft = 0,
        arrowdown = 0,
        arrowright = 0,
        1 = 0,
        2 = 0,
        3 = 0,
        4 = 0,
        5 = 0,
        6 = 0,
        7 = 0,
        8 = 0,
        9 = 0,
        0 = 0
    }
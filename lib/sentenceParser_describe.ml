[@@@ocaml.ppx.context
  {
    tool_name = "ppx_driver";
    include_dirs = [];
    load_path = [];
    open_modules = [];
    for_package = None;
    debug = false;
    use_threads = false;
    use_vmthreads = false;
    recursive_types = false;
    principal = false;
    transparent_modules = false;
    unboxed_types = false;
    unsafe_string = false;
    cookies = [("inline_tests", "enabled"); ("library-name", "gbnf_parser")]
  }]
let () =
  Ppx_module_timer_runtime.record_start Ppx_module_timer_runtime.__MODULE__
let () =
  Ppx_bench_lib.Benchmark_accumulator.Current_libname.set "gbnf_parser"
let () =
  Expect_test_collector.Current_file.set
    ~absolute_filename:"lib/sentenceParser.ml"
let () =
  Ppx_inline_test_lib.set_lib_and_partition "gbnf_parser" "sentenceParser.ml"

module MenhirBasics =
  struct
    exception Error 
    let _eRR : exn = Error
    type token =
      | Tchar of int 
      | STAR 
      | RPAREN 
      | REGEX of string Positions.located 
      | QUESTION 
      | QID of string Positions.located 
      | PLUS 
      | NEWLINE 
      | LPAREN 
      | LID of string Positions.located 
      | EOF 
      | DASH 
      | COLONCOLONEQUAL 
      | CARET 
      | BAR 
  end
include MenhirBasics
open Syntax
module Tables =
  struct
    include MenhirBasics
    let token2terminal : token -> int =
      fun _tok ->
        match _tok with
        | BAR -> 15
        | CARET -> 14
        | COLONCOLONEQUAL -> 13
        | DASH -> 12
        | EOF -> 11
        | LID _ -> 10
        | LPAREN -> 9
        | NEWLINE -> 8
        | PLUS -> 7
        | QID _ -> 6
        | QUESTION -> 5
        | REGEX _ -> 4
        | RPAREN -> 3
        | STAR -> 2
        | Tchar _ -> 1
    and error_terminal = 0
    and token2value : token -> Obj.t =
      fun _tok ->
        match _tok with
        | BAR -> Obj.repr ()
        | CARET -> Obj.repr ()
        | COLONCOLONEQUAL -> Obj.repr ()
        | DASH -> Obj.repr ()
        | EOF -> Obj.repr ()
        | LID _v -> Obj.repr _v
        | LPAREN -> Obj.repr ()
        | NEWLINE -> Obj.repr ()
        | PLUS -> Obj.repr ()
        | QID _v -> Obj.repr _v
        | QUESTION -> Obj.repr ()
        | REGEX _v -> Obj.repr _v
        | RPAREN -> Obj.repr ()
        | STAR -> Obj.repr ()
        | Tchar _v -> Obj.repr _v
    and default_reduction =
      (8,
        "\000\000\019\000\000\000\000\006\028\029\000\000\017\000\030\000\000\007\000\r\012\011\000\026\n\000\t\000\027\000\000\000\000\022\000\000\015\000\023\000\020\025\000\024\001")
    and error =
      (16,
        "\000\160\000\160\000\000\000\004Jb\127\251@\000\000\000\000\000\000\000J\226J\226\000\000Jb\000\000@\000\127\243\000\000\127\243\000\000\000\000\000\000\016\000\000\000\000\000Z\243\000\000\127\243\000\000\016\145J\226JbZ\243\000\000\000\144\000\176\000\000\000 \000\000\000\016\000\000\000\000\000 \000\000\000\000")
    and start = 1
    and action =
      ((8,
         "p\142\000\003v\0032\000\000\000\172\172\000v\0004\028\000:\000\000\000L\000\000v\000X\000\180\172v\148\000\178\178\000@\000Z\000\000@\000\000"),
        (8,
          "\029\029\029\029\029\029\029\029\029\029\029\026\018\029\029F\r\r\r\r\r\r\r\r\r\r\030\022\r\r5N55R5V5555^\01455F\017\017\017\017\017\017\017\017\017\017\163\000\017\017\022\000\t\"\006&\014\t*:\t\000\000>\t\022\000\005\"\006&E\005*:\005\000=>\005=\000=Q.==\142QE=Q\000\000\000z"))
    and lhs = (4, "\r\220\203\187\170\153\153\135veC\"!\017\017")
    and goto =
      ((8,
         "\003\005\000\000\003\000\000\000\000\000\003\016\000\024\000\012\000\000\000\000\000\000\000\000\000:\000\000\000\000\024\":\0004\016\000\"\000\000\000\000>\000\000"),
        (8,
          "\019#*\"\003+\014-\025\026\028\029\030\019\003\r\023\017\019 '\025\026\028\029\030\025!\028\029\019%&(,\000\000\000\027\000\028\029"))
    and semantic_action =
      [|((fun _menhir_env ->
            let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
            let { MenhirLib.EngineTypes.state = _;
                  MenhirLib.EngineTypes.semv = _4;
                  MenhirLib.EngineTypes.startp = _startpos__4_;
                  MenhirLib.EngineTypes.endp = _endpos__4_;
                  MenhirLib.EngineTypes.next =
                    { MenhirLib.EngineTypes.state = _;
                      MenhirLib.EngineTypes.semv = _3;
                      MenhirLib.EngineTypes.startp = _startpos__3_;
                      MenhirLib.EngineTypes.endp = _endpos__3_;
                      MenhirLib.EngineTypes.next =
                        { MenhirLib.EngineTypes.state = _;
                          MenhirLib.EngineTypes.semv = _2;
                          MenhirLib.EngineTypes.startp = _startpos__2_;
                          MenhirLib.EngineTypes.endp = _endpos__2_;
                          MenhirLib.EngineTypes.next =
                            { MenhirLib.EngineTypes.state = _menhir_s;
                              MenhirLib.EngineTypes.semv = _1;
                              MenhirLib.EngineTypes.startp = _startpos__1_;
                              MenhirLib.EngineTypes.endp = _endpos__1_;
                              MenhirLib.EngineTypes.next = _menhir_stack }
                          }
                      }
                  }
              = _menhir_stack in
            let _4 : Syntax.myfactor = Obj.magic _4 in
            let _3 : unit list = Obj.magic _3 in
            let _2 : unit = Obj.magic _2 in
            let _1 : Syntax.myfactor = Obj.magic _1 in
            let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : Syntax.myfactor = NFactor _1 in
            {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = (Obj.repr _v);
              MenhirLib.EngineTypes.startp = _startpos;
              MenhirLib.EngineTypes.endp = _endpos;
              MenhirLib.EngineTypes.next = _menhir_stack
            }));((fun _menhir_env ->
                    let _menhir_stack =
                      _menhir_env.MenhirLib.EngineTypes.stack in
                    let { MenhirLib.EngineTypes.state = _menhir_s;
                          MenhirLib.EngineTypes.semv = _1;
                          MenhirLib.EngineTypes.startp = _startpos__1_;
                          MenhirLib.EngineTypes.endp = _endpos__1_;
                          MenhirLib.EngineTypes.next = _menhir_stack }
                      = _menhir_stack in
                    let _1 : Syntax.myfactor = Obj.magic _1 in
                    let _endpos__0_ =
                      _menhir_stack.MenhirLib.EngineTypes.endp in
                    let _startpos = _startpos__1_ in
                    let _endpos = _endpos__1_ in
                    let _v : Syntax.myfactor =
                      print_endline (Batteries.dump ("DEBUG:alt", _1));
                      NFactor _1 in
                    {
                      MenhirLib.EngineTypes.state = _menhir_s;
                      MenhirLib.EngineTypes.semv = (Obj.repr _v);
                      MenhirLib.EngineTypes.startp = _startpos;
                      MenhirLib.EngineTypes.endp = _endpos;
                      MenhirLib.EngineTypes.next = _menhir_stack
                    }));((fun _menhir_env ->
                            let _menhir_stack =
                              _menhir_env.MenhirLib.EngineTypes.stack in
                            let { MenhirLib.EngineTypes.state = _;
                                  MenhirLib.EngineTypes.semv = _2;
                                  MenhirLib.EngineTypes.startp =
                                    _startpos__2_;
                                  MenhirLib.EngineTypes.endp = _endpos__2_;
                                  MenhirLib.EngineTypes.next =
                                    {
                                      MenhirLib.EngineTypes.state = _menhir_s;
                                      MenhirLib.EngineTypes.semv = _1;
                                      MenhirLib.EngineTypes.startp =
                                        _startpos__1_;
                                      MenhirLib.EngineTypes.endp =
                                        _endpos__1_;
                                      MenhirLib.EngineTypes.next =
                                        _menhir_stack
                                      }
                                  }
                              = _menhir_stack in
                            let _2 : Syntax.myfactor = Obj.magic _2 in
                            let _1 : unit = Obj.magic _1 in
                            let _endpos__0_ =
                              _menhir_stack.MenhirLib.EngineTypes.endp in
                            let _startpos = _startpos__1_ in
                            let _endpos = _endpos__2_ in
                            let _v : Syntax.myfactor =
                              print_endline
                                (Batteries.dump ("DEBUG:ccrs", _2));
                              NFactor _2 in
                            {
                              MenhirLib.EngineTypes.state = _menhir_s;
                              MenhirLib.EngineTypes.semv = (Obj.repr _v);
                              MenhirLib.EngineTypes.startp = _startpos;
                              MenhirLib.EngineTypes.endp = _endpos;
                              MenhirLib.EngineTypes.next = _menhir_stack
                            }));((fun _menhir_env ->
                                    let _menhir_stack =
                                      _menhir_env.MenhirLib.EngineTypes.stack in
                                    let {
                                          MenhirLib.EngineTypes.state =
                                            _menhir_s;
                                          MenhirLib.EngineTypes.semv = _1;
                                          MenhirLib.EngineTypes.startp =
                                            _startpos__1_;
                                          MenhirLib.EngineTypes.endp =
                                            _endpos__1_;
                                          MenhirLib.EngineTypes.next =
                                            _menhir_stack
                                          }
                                      = _menhir_stack in
                                    let _1 : Syntax.myfactor = Obj.magic _1 in
                                    let _endpos__0_ =
                                      _menhir_stack.MenhirLib.EngineTypes.endp in
                                    let _startpos = _startpos__1_ in
                                    let _endpos = _endpos__1_ in
                                    let _v : Syntax.myfactor =
                                      print_endline
                                        (Batteries.dump ("DEBUG:cc2rs", _1));
                                      CharClass in
                                    {
                                      MenhirLib.EngineTypes.state = _menhir_s;
                                      MenhirLib.EngineTypes.semv =
                                        (Obj.repr _v);
                                      MenhirLib.EngineTypes.startp =
                                        _startpos;
                                      MenhirLib.EngineTypes.endp = _endpos;
                                      MenhirLib.EngineTypes.next =
                                        _menhir_stack
                                    }));((fun _menhir_env ->
                                            let _menhir_stack =
                                              _menhir_env.MenhirLib.EngineTypes.stack in
                                            let {
                                                  MenhirLib.EngineTypes.state
                                                    = _;
                                                  MenhirLib.EngineTypes.semv
                                                    = _3;
                                                  MenhirLib.EngineTypes.startp
                                                    = _startpos__3_;
                                                  MenhirLib.EngineTypes.endp
                                                    = _endpos__3_;
                                                  MenhirLib.EngineTypes.next
                                                    =
                                                    {
                                                      MenhirLib.EngineTypes.state
                                                        = _;
                                                      MenhirLib.EngineTypes.semv
                                                        = _2;
                                                      MenhirLib.EngineTypes.startp
                                                        = _startpos__2_;
                                                      MenhirLib.EngineTypes.endp
                                                        = _endpos__2_;
                                                      MenhirLib.EngineTypes.next
                                                        =
                                                        {
                                                          MenhirLib.EngineTypes.state
                                                            = _menhir_s;
                                                          MenhirLib.EngineTypes.semv
                                                            = _1;
                                                          MenhirLib.EngineTypes.startp
                                                            = _startpos__1_;
                                                          MenhirLib.EngineTypes.endp
                                                            = _endpos__1_;
                                                          MenhirLib.EngineTypes.next
                                                            = _menhir_stack
                                                          }
                                                      }
                                                  }
                                              = _menhir_stack in
                                            let _3 : int = Obj.magic _3 in
                                            let _2 : unit = Obj.magic _2 in
                                            let _1 : int = Obj.magic _1 in
                                            let _endpos__0_ =
                                              _menhir_stack.MenhirLib.EngineTypes.endp in
                                            let _startpos = _startpos__1_ in
                                            let _endpos = _endpos__3_ in
                                            let _v : Syntax.myfactor =
                                              print_endline
                                                (Batteries.dump
                                                   ("DEBUG:cc3rs", _1, _2));
                                              CharInt _1 in
                                            {
                                              MenhirLib.EngineTypes.state =
                                                _menhir_s;
                                              MenhirLib.EngineTypes.semv =
                                                (Obj.repr _v);
                                              MenhirLib.EngineTypes.startp =
                                                _startpos;
                                              MenhirLib.EngineTypes.endp =
                                                _endpos;
                                              MenhirLib.EngineTypes.next =
                                                _menhir_stack
                                            }));((fun _menhir_env ->
                                                    let _menhir_stack =
                                                      _menhir_env.MenhirLib.EngineTypes.stack in
                                                    let {
                                                          MenhirLib.EngineTypes.state
                                                            = _;
                                                          MenhirLib.EngineTypes.semv
                                                            = _2;
                                                          MenhirLib.EngineTypes.startp
                                                            = _startpos__2_;
                                                          MenhirLib.EngineTypes.endp
                                                            = _endpos__2_;
                                                          MenhirLib.EngineTypes.next
                                                            =
                                                            {
                                                              MenhirLib.EngineTypes.state
                                                                = _menhir_s;
                                                              MenhirLib.EngineTypes.semv
                                                                = _1;
                                                              MenhirLib.EngineTypes.startp
                                                                =
                                                                _startpos__1_;
                                                              MenhirLib.EngineTypes.endp
                                                                = _endpos__1_;
                                                              MenhirLib.EngineTypes.next
                                                                =
                                                                _menhir_stack
                                                              }
                                                          }
                                                      = _menhir_stack in
                                                    let _2 : int =
                                                      Obj.magic _2 in
                                                    let _1 : Syntax.myfactor
                                                      = Obj.magic _1 in
                                                    let _endpos__0_ =
                                                      _menhir_stack.MenhirLib.EngineTypes.endp in
                                                    let _startpos =
                                                      _startpos__1_ in
                                                    let _endpos = _endpos__2_ in
                                                    let _v : Syntax.myfactor
                                                      =
                                                      print_endline
                                                        (Batteries.dump
                                                           ("DEBUG:cc4rs",
                                                             _1));
                                                      NFactor _1 in
                                                    {
                                                      MenhirLib.EngineTypes.state
                                                        = _menhir_s;
                                                      MenhirLib.EngineTypes.semv
                                                        = (Obj.repr _v);
                                                      MenhirLib.EngineTypes.startp
                                                        = _startpos;
                                                      MenhirLib.EngineTypes.endp
                                                        = _endpos;
                                                      MenhirLib.EngineTypes.next
                                                        = _menhir_stack
                                                    }));((fun _menhir_env ->
                                                            let _menhir_stack
                                                              =
                                                              _menhir_env.MenhirLib.EngineTypes.stack in
                                                            let {
                                                                  MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                  MenhirLib.EngineTypes.semv
                                                                    = _1;
                                                                  MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos__1_;
                                                                  MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos__1_;
                                                                  MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                  }
                                                              = _menhir_stack in
                                                            let _1 : 
                                                              int =
                                                              Obj.magic _1 in
                                                            let _endpos__0_ =
                                                              _menhir_stack.MenhirLib.EngineTypes.endp in
                                                            let _startpos =
                                                              _startpos__1_ in
                                                            let _endpos =
                                                              _endpos__1_ in
                                                            let _v :
                                                              Syntax.myfactor
                                                              =
                                                              print_endline
                                                                (Batteries.dump
                                                                   ("DEBUG:cc5rs",
                                                                    _1));
                                                              CharInt _1 in
                                                            {
                                                              MenhirLib.EngineTypes.state
                                                                = _menhir_s;
                                                              MenhirLib.EngineTypes.semv
                                                                =
                                                                (Obj.repr _v);
                                                              MenhirLib.EngineTypes.startp
                                                                = _startpos;
                                                              MenhirLib.EngineTypes.endp
                                                                = _endpos;
                                                              MenhirLib.EngineTypes.next
                                                                =
                                                                _menhir_stack
                                                            }));((fun
                                                                    _menhir_env
                                                                    ->
                                                                    let _menhir_stack
                                                                    =
                                                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                                                    let 
                                                                    {
                                                                    MenhirLib.EngineTypes.state
                                                                    = _;
                                                                    MenhirLib.EngineTypes.semv
                                                                    = _2;
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos__2_;
                                                                    MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos__2_;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    = _1;
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos__1_;
                                                                    MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos__1_;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                    } } =
                                                                    _menhir_stack in
                                                                    let _2 :
                                                                    Syntax.myfactor
                                                                    =
                                                                    Obj.magic
                                                                    _2 in
                                                                    let _1 :
                                                                    Syntax.myfactor
                                                                    =
                                                                    Obj.magic
                                                                    _1 in
                                                                    let _endpos__0_
                                                                    =
                                                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                                                    let _startpos
                                                                    =
                                                                    _startpos__1_ in
                                                                    let _endpos
                                                                    =
                                                                    _endpos__2_ in
                                                                    let _v :
                                                                    Syntax.myfactor
                                                                    =
                                                                    print_endline
                                                                    (Batteries.dump
                                                                    ("DEBUG:concat1",
                                                                    _1));
                                                                    NFactor
                                                                    _1 in
                                                                    {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    =
                                                                    (Obj.repr
                                                                    _v);
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos;
                                                                    MenhirLib.EngineTypes.endp
                                                                    = _endpos;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                    }));((
        fun _menhir_env ->
          let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
          let { MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = _1;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack }
            = _menhir_stack in
          let _1 : Syntax.myfactor = Obj.magic _1 in
          let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
          let _startpos = _startpos__1_ in
          let _endpos = _endpos__1_ in
          let _v : Syntax.myfactor =
            print_endline (Batteries.dump ("DEBUG:concat2", _1)); NFactor _1 in
          {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = (Obj.repr _v);
            MenhirLib.EngineTypes.startp = _startpos;
            MenhirLib.EngineTypes.endp = _endpos;
            MenhirLib.EngineTypes.next = _menhir_stack
          }));((fun _menhir_env ->
                  let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
                  let { MenhirLib.EngineTypes.state = _;
                        MenhirLib.EngineTypes.semv = _1_inlined1;
                        MenhirLib.EngineTypes.startp = _startpos__1_inlined1_;
                        MenhirLib.EngineTypes.endp = _endpos__1_inlined1_;
                        MenhirLib.EngineTypes.next =
                          { MenhirLib.EngineTypes.state = _menhir_s;
                            MenhirLib.EngineTypes.semv = _1;
                            MenhirLib.EngineTypes.startp = _startpos__1_;
                            MenhirLib.EngineTypes.endp = _endpos__1_;
                            MenhirLib.EngineTypes.next = _menhir_stack }
                        }
                    = _menhir_stack in
                  let _1_inlined1 : unit = Obj.magic _1_inlined1 in
                  let _1 : Syntax.myfactor = Obj.magic _1 in
                  let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
                  let _startpos = _startpos__1_ in
                  let _endpos = _endpos__1_inlined1_ in
                  let _v : Syntax.myfactor =
                    let _2 =
                      let _1 = _1_inlined1 in
                      let _1 =
                        print_endline (Batteries.dump ("DEBUG:plus", _1));
                        Plus in
                      print_endline (Batteries.dump ("DEBUG:mod", _1));
                      NFactor _1 in
                    let _1 =
                      print_endline (Batteries.dump ("DEBUG:termfactor", _1));
                      NFactor _1 in
                    NFactor _1 in
                  {
                    MenhirLib.EngineTypes.state = _menhir_s;
                    MenhirLib.EngineTypes.semv = (Obj.repr _v);
                    MenhirLib.EngineTypes.startp = _startpos;
                    MenhirLib.EngineTypes.endp = _endpos;
                    MenhirLib.EngineTypes.next = _menhir_stack
                  }));((fun _menhir_env ->
                          let _menhir_stack =
                            _menhir_env.MenhirLib.EngineTypes.stack in
                          let { MenhirLib.EngineTypes.state = _;
                                MenhirLib.EngineTypes.semv = _1_inlined1;
                                MenhirLib.EngineTypes.startp =
                                  _startpos__1_inlined1_;
                                MenhirLib.EngineTypes.endp =
                                  _endpos__1_inlined1_;
                                MenhirLib.EngineTypes.next =
                                  { MenhirLib.EngineTypes.state = _menhir_s;
                                    MenhirLib.EngineTypes.semv = _1;
                                    MenhirLib.EngineTypes.startp =
                                      _startpos__1_;
                                    MenhirLib.EngineTypes.endp = _endpos__1_;
                                    MenhirLib.EngineTypes.next =
                                      _menhir_stack
                                    }
                                }
                            = _menhir_stack in
                          let _1_inlined1 : unit = Obj.magic _1_inlined1 in
                          let _1 : Syntax.myfactor = Obj.magic _1 in
                          let _endpos__0_ =
                            _menhir_stack.MenhirLib.EngineTypes.endp in
                          let _startpos = _startpos__1_ in
                          let _endpos = _endpos__1_inlined1_ in
                          let _v : Syntax.myfactor =
                            let _2 =
                              let _1 = _1_inlined1 in
                              let _1 =
                                print_endline
                                  (Batteries.dump ("DEBUG:quest", _1));
                                Question in
                              print_endline
                                (Batteries.dump ("DEBUG:quest", _1));
                              NFactor _1 in
                            let _1 =
                              print_endline
                                (Batteries.dump ("DEBUG:termfactor", _1));
                              NFactor _1 in
                            NFactor _1 in
                          {
                            MenhirLib.EngineTypes.state = _menhir_s;
                            MenhirLib.EngineTypes.semv = (Obj.repr _v);
                            MenhirLib.EngineTypes.startp = _startpos;
                            MenhirLib.EngineTypes.endp = _endpos;
                            MenhirLib.EngineTypes.next = _menhir_stack
                          }));((fun _menhir_env ->
                                  let _menhir_stack =
                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                  let { MenhirLib.EngineTypes.state = _;
                                        MenhirLib.EngineTypes.semv =
                                          _1_inlined1;
                                        MenhirLib.EngineTypes.startp =
                                          _startpos__1_inlined1_;
                                        MenhirLib.EngineTypes.endp =
                                          _endpos__1_inlined1_;
                                        MenhirLib.EngineTypes.next =
                                          {
                                            MenhirLib.EngineTypes.state =
                                              _menhir_s;
                                            MenhirLib.EngineTypes.semv = _1;
                                            MenhirLib.EngineTypes.startp =
                                              _startpos__1_;
                                            MenhirLib.EngineTypes.endp =
                                              _endpos__1_;
                                            MenhirLib.EngineTypes.next =
                                              _menhir_stack
                                            }
                                        }
                                    = _menhir_stack in
                                  let _1_inlined1 : unit =
                                    Obj.magic _1_inlined1 in
                                  let _1 : Syntax.myfactor = Obj.magic _1 in
                                  let _endpos__0_ =
                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                  let _startpos = _startpos__1_ in
                                  let _endpos = _endpos__1_inlined1_ in
                                  let _v : Syntax.myfactor =
                                    let _2 =
                                      let _1 = Star in
                                      print_endline
                                        (Batteries.dump ("DEBUG:star", _1));
                                      NFactor _1 in
                                    let _1 =
                                      print_endline
                                        (Batteries.dump
                                           ("DEBUG:termfactor", _1));
                                      NFactor _1 in
                                    NFactor _1 in
                                  {
                                    MenhirLib.EngineTypes.state = _menhir_s;
                                    MenhirLib.EngineTypes.semv =
                                      (Obj.repr _v);
                                    MenhirLib.EngineTypes.startp = _startpos;
                                    MenhirLib.EngineTypes.endp = _endpos;
                                    MenhirLib.EngineTypes.next =
                                      _menhir_stack
                                  }));((fun _menhir_env ->
                                          let _menhir_stack =
                                            _menhir_env.MenhirLib.EngineTypes.stack in
                                          let {
                                                MenhirLib.EngineTypes.state =
                                                  _menhir_s;
                                                MenhirLib.EngineTypes.semv =
                                                  _1;
                                                MenhirLib.EngineTypes.startp
                                                  = _startpos__1_;
                                                MenhirLib.EngineTypes.endp =
                                                  _endpos__1_;
                                                MenhirLib.EngineTypes.next =
                                                  _menhir_stack
                                                }
                                            = _menhir_stack in
                                          let _1 : Syntax.myfactor =
                                            Obj.magic _1 in
                                          let _endpos__0_ =
                                            _menhir_stack.MenhirLib.EngineTypes.endp in
                                          let _startpos = _startpos__1_ in
                                          let _endpos = _endpos__1_ in
                                          let _v : Syntax.myfactor =
                                            let _1 =
                                              print_endline
                                                (Batteries.dump
                                                   ("DEBUG:termfactor", _1));
                                              NFactor _1 in
                                            NFactor _1 in
                                          {
                                            MenhirLib.EngineTypes.state =
                                              _menhir_s;
                                            MenhirLib.EngineTypes.semv =
                                              (Obj.repr _v);
                                            MenhirLib.EngineTypes.startp =
                                              _startpos;
                                            MenhirLib.EngineTypes.endp =
                                              _endpos;
                                            MenhirLib.EngineTypes.next =
                                              _menhir_stack
                                          }));((fun _menhir_env ->
                                                  let _menhir_stack =
                                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                                  let {
                                                        MenhirLib.EngineTypes.state
                                                          = _;
                                                        MenhirLib.EngineTypes.semv
                                                          = _2;
                                                        MenhirLib.EngineTypes.startp
                                                          = _startpos__2_;
                                                        MenhirLib.EngineTypes.endp
                                                          = _endpos__2_;
                                                        MenhirLib.EngineTypes.next
                                                          =
                                                          {
                                                            MenhirLib.EngineTypes.state
                                                              = _menhir_s;
                                                            MenhirLib.EngineTypes.semv
                                                              = rs;
                                                            MenhirLib.EngineTypes.startp
                                                              = _startpos_rs_;
                                                            MenhirLib.EngineTypes.endp
                                                              = _endpos_rs_;
                                                            MenhirLib.EngineTypes.next
                                                              = _menhir_stack
                                                            }
                                                        }
                                                    = _menhir_stack in
                                                  let _2 : unit =
                                                    Obj.magic _2 in
                                                  let rs : Syntax.myfactor =
                                                    Obj.magic rs in
                                                  let _endpos__0_ =
                                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                                  let _startpos =
                                                    _startpos_rs_ in
                                                  let _endpos = _endpos__2_ in
                                                  let _v :
                                                    Syntax.partial_grammar =
                                                    print_endline
                                                      (Batteries.dump
                                                         ("DEBUG:grammar",
                                                           rs, _2));
                                                    {
                                                      pg_filename = "";
                                                      pg_rules = []
                                                    } in
                                                  {
                                                    MenhirLib.EngineTypes.state
                                                      = _menhir_s;
                                                    MenhirLib.EngineTypes.semv
                                                      = (Obj.repr _v);
                                                    MenhirLib.EngineTypes.startp
                                                      = _startpos;
                                                    MenhirLib.EngineTypes.endp
                                                      = _endpos;
                                                    MenhirLib.EngineTypes.next
                                                      = _menhir_stack
                                                  }));((fun _menhir_env ->
                                                          let _menhir_stack =
                                                            _menhir_env.MenhirLib.EngineTypes.stack in
                                                          let _menhir_s =
                                                            _menhir_env.MenhirLib.EngineTypes.current in
                                                          let _endpos__0_ =
                                                            _menhir_stack.MenhirLib.EngineTypes.endp in
                                                          let _startpos =
                                                            _menhir_stack.MenhirLib.EngineTypes.endp in
                                                          let _endpos =
                                                            _startpos in
                                                          let _v : unit list
                                                            = [] in
                                                          {
                                                            MenhirLib.EngineTypes.state
                                                              = _menhir_s;
                                                            MenhirLib.EngineTypes.semv
                                                              = (Obj.repr _v);
                                                            MenhirLib.EngineTypes.startp
                                                              = _startpos;
                                                            MenhirLib.EngineTypes.endp
                                                              = _endpos;
                                                            MenhirLib.EngineTypes.next
                                                              = _menhir_stack
                                                          }));((fun
                                                                  _menhir_env
                                                                  ->
                                                                  let _menhir_stack
                                                                    =
                                                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                                                  let 
                                                                    {
                                                                    MenhirLib.EngineTypes.state
                                                                    = _;
                                                                    MenhirLib.EngineTypes.semv
                                                                    = xs;
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos_xs_;
                                                                    MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos_xs_;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    = x;
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos_x_;
                                                                    MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos_x_;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                    } } =
                                                                    _menhir_stack in
                                                                  let xs :
                                                                    unit list
                                                                    =
                                                                    Obj.magic
                                                                    xs in
                                                                  let x :
                                                                    unit =
                                                                    Obj.magic
                                                                    x in
                                                                  let _endpos__0_
                                                                    =
                                                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                                                  let _startpos
                                                                    =
                                                                    _startpos_x_ in
                                                                  let _endpos
                                                                    =
                                                                    _endpos_xs_ in
                                                                  let _v :
                                                                    unit list
                                                                    = x :: xs in
                                                                  {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    =
                                                                    (Obj.repr
                                                                    _v);
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos;
                                                                    MenhirLib.EngineTypes.endp
                                                                    = _endpos;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                  }));((
        fun _menhir_env ->
          let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
          let { MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = x;
                MenhirLib.EngineTypes.startp = _startpos_x_;
                MenhirLib.EngineTypes.endp = _endpos_x_;
                MenhirLib.EngineTypes.next = _menhir_stack }
            = _menhir_stack in
          let x : unit = Obj.magic x in
          let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
          let _startpos = _startpos_x_ in
          let _endpos = _endpos_x_ in
          let _v : unit list = [x] in
          {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = (Obj.repr _v);
            MenhirLib.EngineTypes.startp = _startpos;
            MenhirLib.EngineTypes.endp = _endpos;
            MenhirLib.EngineTypes.next = _menhir_stack
          }));((fun _menhir_env ->
                  let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
                  let { MenhirLib.EngineTypes.state = _;
                        MenhirLib.EngineTypes.semv = xs;
                        MenhirLib.EngineTypes.startp = _startpos_xs_;
                        MenhirLib.EngineTypes.endp = _endpos_xs_;
                        MenhirLib.EngineTypes.next =
                          { MenhirLib.EngineTypes.state = _menhir_s;
                            MenhirLib.EngineTypes.semv = x;
                            MenhirLib.EngineTypes.startp = _startpos_x_;
                            MenhirLib.EngineTypes.endp = _endpos_x_;
                            MenhirLib.EngineTypes.next = _menhir_stack }
                        }
                    = _menhir_stack in
                  let xs : unit list = Obj.magic xs in
                  let x : unit = Obj.magic x in
                  let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
                  let _startpos = _startpos_x_ in
                  let _endpos = _endpos_xs_ in
                  let _v : unit list = x :: xs in
                  {
                    MenhirLib.EngineTypes.state = _menhir_s;
                    MenhirLib.EngineTypes.semv = (Obj.repr _v);
                    MenhirLib.EngineTypes.startp = _startpos;
                    MenhirLib.EngineTypes.endp = _endpos;
                    MenhirLib.EngineTypes.next = _menhir_stack
                  }));((fun _menhir_env ->
                          let _menhir_stack =
                            _menhir_env.MenhirLib.EngineTypes.stack in
                          let { MenhirLib.EngineTypes.state = _;
                                MenhirLib.EngineTypes.semv = _2;
                                MenhirLib.EngineTypes.startp = _startpos__2_;
                                MenhirLib.EngineTypes.endp = _endpos__2_;
                                MenhirLib.EngineTypes.next =
                                  { MenhirLib.EngineTypes.state = _menhir_s;
                                    MenhirLib.EngineTypes.semv = _1;
                                    MenhirLib.EngineTypes.startp =
                                      _startpos__1_;
                                    MenhirLib.EngineTypes.endp = _endpos__1_;
                                    MenhirLib.EngineTypes.next =
                                      _menhir_stack
                                    }
                                }
                            = _menhir_stack in
                          let _2 : unit = Obj.magic _2 in
                          let _1 : unit list = Obj.magic _1 in
                          let _endpos__0_ =
                            _menhir_stack.MenhirLib.EngineTypes.endp in
                          let _startpos = _startpos__1_ in
                          let _endpos = _endpos__2_ in
                          let _v : unit =
                            print_endline (Batteries.dump "DEBUG:DONE") in
                          {
                            MenhirLib.EngineTypes.state = _menhir_s;
                            MenhirLib.EngineTypes.semv = (Obj.repr _v);
                            MenhirLib.EngineTypes.startp = _startpos;
                            MenhirLib.EngineTypes.endp = _endpos;
                            MenhirLib.EngineTypes.next = _menhir_stack
                          }));((fun _menhir_env ->
                                  let _menhir_stack =
                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                  let {
                                        MenhirLib.EngineTypes.state =
                                          _menhir_s;
                                        MenhirLib.EngineTypes.semv = _1;
                                        MenhirLib.EngineTypes.startp =
                                          _startpos__1_;
                                        MenhirLib.EngineTypes.endp =
                                          _endpos__1_;
                                        MenhirLib.EngineTypes.next =
                                          _menhir_stack
                                        }
                                    = _menhir_stack in
                                  let _1 : Syntax.myfactor = Obj.magic _1 in
                                  let _endpos__0_ =
                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                  let _startpos = _startpos__1_ in
                                  let _endpos = _endpos__1_ in
                                  let _v : Syntax.myfactor =
                                    print_endline
                                      (Batteries.dump ("DEBUG:rhs", _1));
                                    NFactor _1 in
                                  {
                                    MenhirLib.EngineTypes.state = _menhir_s;
                                    MenhirLib.EngineTypes.semv =
                                      (Obj.repr _v);
                                    MenhirLib.EngineTypes.startp = _startpos;
                                    MenhirLib.EngineTypes.endp = _endpos;
                                    MenhirLib.EngineTypes.next =
                                      _menhir_stack
                                  }));((fun _menhir_env ->
                                          let _menhir_stack =
                                            _menhir_env.MenhirLib.EngineTypes.stack in
                                          let {
                                                MenhirLib.EngineTypes.state =
                                                  _;
                                                MenhirLib.EngineTypes.semv =
                                                  branches;
                                                MenhirLib.EngineTypes.startp
                                                  = _startpos_branches_;
                                                MenhirLib.EngineTypes.endp =
                                                  _endpos_branches_;
                                                MenhirLib.EngineTypes.next =
                                                  {
                                                    MenhirLib.EngineTypes.state
                                                      = _;
                                                    MenhirLib.EngineTypes.semv
                                                      = _2;
                                                    MenhirLib.EngineTypes.startp
                                                      = _startpos__2_;
                                                    MenhirLib.EngineTypes.endp
                                                      = _endpos__2_;
                                                    MenhirLib.EngineTypes.next
                                                      =
                                                      {
                                                        MenhirLib.EngineTypes.state
                                                          = _menhir_s;
                                                        MenhirLib.EngineTypes.semv
                                                          = symbol;
                                                        MenhirLib.EngineTypes.startp
                                                          = _startpos_symbol_;
                                                        MenhirLib.EngineTypes.endp
                                                          = _endpos_symbol_;
                                                        MenhirLib.EngineTypes.next
                                                          = _menhir_stack
                                                        }
                                                    }
                                                }
                                            = _menhir_stack in
                                          let branches : Syntax.myfactor =
                                            Obj.magic branches in
                                          let _2 : unit = Obj.magic _2 in
                                          let symbol :
                                            string Positions.located =
                                            Obj.magic symbol in
                                          let _endpos__0_ =
                                            _menhir_stack.MenhirLib.EngineTypes.endp in
                                          let _startpos = _startpos_symbol_ in
                                          let _endpos = _endpos_branches_ in
                                          let _v : Syntax.parameterized_rule
                                            =
                                            print_endline
                                              (Batteries.dump
                                                 ("DEBUG:rule", symbol,
                                                   branches));
                                            {
                                              pr_nt =
                                                (Positions.value symbol);
                                              pr_positions =
                                                [Positions.position symbol];
                                              pr_branches = []
                                            } in
                                          {
                                            MenhirLib.EngineTypes.state =
                                              _menhir_s;
                                            MenhirLib.EngineTypes.semv =
                                              (Obj.repr _v);
                                            MenhirLib.EngineTypes.startp =
                                              _startpos;
                                            MenhirLib.EngineTypes.endp =
                                              _endpos;
                                            MenhirLib.EngineTypes.next =
                                              _menhir_stack
                                          }));((fun _menhir_env ->
                                                  let _menhir_stack =
                                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                                  let {
                                                        MenhirLib.EngineTypes.state
                                                          = _;
                                                        MenhirLib.EngineTypes.semv
                                                          = _3;
                                                        MenhirLib.EngineTypes.startp
                                                          = _startpos__3_;
                                                        MenhirLib.EngineTypes.endp
                                                          = _endpos__3_;
                                                        MenhirLib.EngineTypes.next
                                                          =
                                                          {
                                                            MenhirLib.EngineTypes.state
                                                              = _;
                                                            MenhirLib.EngineTypes.semv
                                                              = _2;
                                                            MenhirLib.EngineTypes.startp
                                                              = _startpos__2_;
                                                            MenhirLib.EngineTypes.endp
                                                              = _endpos__2_;
                                                            MenhirLib.EngineTypes.next
                                                              =
                                                              {
                                                                MenhirLib.EngineTypes.state
                                                                  = _menhir_s;
                                                                MenhirLib.EngineTypes.semv
                                                                  = _1;
                                                                MenhirLib.EngineTypes.startp
                                                                  =
                                                                  _startpos__1_;
                                                                MenhirLib.EngineTypes.endp
                                                                  =
                                                                  _endpos__1_;
                                                                MenhirLib.EngineTypes.next
                                                                  =
                                                                  _menhir_stack
                                                                }
                                                            }
                                                        }
                                                    = _menhir_stack in
                                                  let _3 :
                                                    Syntax.parameterized_rule
                                                    = Obj.magic _3 in
                                                  let _2 : unit list =
                                                    Obj.magic _2 in
                                                  let _1 : Syntax.myfactor =
                                                    Obj.magic _1 in
                                                  let _endpos__0_ =
                                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                                  let _startpos =
                                                    _startpos__1_ in
                                                  let _endpos = _endpos__3_ in
                                                  let _v : Syntax.myfactor =
                                                    print_endline
                                                      (Batteries.dump
                                                         ("DEBUG:OLDRULE1",
                                                           _3));
                                                    Rule _3 in
                                                  {
                                                    MenhirLib.EngineTypes.state
                                                      = _menhir_s;
                                                    MenhirLib.EngineTypes.semv
                                                      = (Obj.repr _v);
                                                    MenhirLib.EngineTypes.startp
                                                      = _startpos;
                                                    MenhirLib.EngineTypes.endp
                                                      = _endpos;
                                                    MenhirLib.EngineTypes.next
                                                      = _menhir_stack
                                                  }));((fun _menhir_env ->
                                                          let _menhir_stack =
                                                            _menhir_env.MenhirLib.EngineTypes.stack in
                                                          let {
                                                                MenhirLib.EngineTypes.state
                                                                  = _;
                                                                MenhirLib.EngineTypes.semv
                                                                  = _2;
                                                                MenhirLib.EngineTypes.startp
                                                                  =
                                                                  _startpos__2_;
                                                                MenhirLib.EngineTypes.endp
                                                                  =
                                                                  _endpos__2_;
                                                                MenhirLib.EngineTypes.next
                                                                  =
                                                                  {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    = _1;
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos__1_;
                                                                    MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos__1_;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                    }
                                                                }
                                                            = _menhir_stack in
                                                          let _2 :
                                                            Syntax.parameterized_rule
                                                            = Obj.magic _2 in
                                                          let _1 : unit list
                                                            = Obj.magic _1 in
                                                          let _endpos__0_ =
                                                            _menhir_stack.MenhirLib.EngineTypes.endp in
                                                          let _startpos =
                                                            _startpos__1_ in
                                                          let _endpos =
                                                            _endpos__2_ in
                                                          let _v :
                                                            Syntax.myfactor =
                                                            print_endline
                                                              (Batteries.dump
                                                                 ("DEBUG:OLDRULE",
                                                                   _1));
                                                            Rule _2 in
                                                          {
                                                            MenhirLib.EngineTypes.state
                                                              = _menhir_s;
                                                            MenhirLib.EngineTypes.semv
                                                              = (Obj.repr _v);
                                                            MenhirLib.EngineTypes.startp
                                                              = _startpos;
                                                            MenhirLib.EngineTypes.endp
                                                              = _endpos;
                                                            MenhirLib.EngineTypes.next
                                                              = _menhir_stack
                                                          }));((fun
                                                                  _menhir_env
                                                                  ->
                                                                  let _menhir_stack
                                                                    =
                                                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                                                  let 
                                                                    {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    = _1;
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos__1_;
                                                                    MenhirLib.EngineTypes.endp
                                                                    =
                                                                    _endpos__1_;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                    } =
                                                                    _menhir_stack in
                                                                  let _1 :
                                                                    Syntax.parameterized_rule
                                                                    =
                                                                    Obj.magic
                                                                    _1 in
                                                                  let _endpos__0_
                                                                    =
                                                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                                                  let _startpos
                                                                    =
                                                                    _startpos__1_ in
                                                                  let _endpos
                                                                    =
                                                                    _endpos__1_ in
                                                                  let _v :
                                                                    Syntax.myfactor
                                                                    =
                                                                    print_endline
                                                                    (Batteries.dump
                                                                    ("DEBUG:OLDRULE",
                                                                    _1));
                                                                    Rule _1 in
                                                                  {
                                                                    MenhirLib.EngineTypes.state
                                                                    =
                                                                    _menhir_s;
                                                                    MenhirLib.EngineTypes.semv
                                                                    =
                                                                    (Obj.repr
                                                                    _v);
                                                                    MenhirLib.EngineTypes.startp
                                                                    =
                                                                    _startpos;
                                                                    MenhirLib.EngineTypes.endp
                                                                    = _endpos;
                                                                    MenhirLib.EngineTypes.next
                                                                    =
                                                                    _menhir_stack
                                                                  }));((
        fun _menhir_env ->
          let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
          let { MenhirLib.EngineTypes.state = _;
                MenhirLib.EngineTypes.semv = _4;
                MenhirLib.EngineTypes.startp = _startpos__4_;
                MenhirLib.EngineTypes.endp = _endpos__4_;
                MenhirLib.EngineTypes.next =
                  { MenhirLib.EngineTypes.state = _;
                    MenhirLib.EngineTypes.semv = _3;
                    MenhirLib.EngineTypes.startp = _startpos__3_;
                    MenhirLib.EngineTypes.endp = _endpos__3_;
                    MenhirLib.EngineTypes.next =
                      { MenhirLib.EngineTypes.state = _;
                        MenhirLib.EngineTypes.semv = _2;
                        MenhirLib.EngineTypes.startp = _startpos__2_;
                        MenhirLib.EngineTypes.endp = _endpos__2_;
                        MenhirLib.EngineTypes.next =
                          { MenhirLib.EngineTypes.state = _menhir_s;
                            MenhirLib.EngineTypes.semv = _1;
                            MenhirLib.EngineTypes.startp = _startpos__1_;
                            MenhirLib.EngineTypes.endp = _endpos__1_;
                            MenhirLib.EngineTypes.next = _menhir_stack }
                        }
                    }
                }
            = _menhir_stack in
          let _4 : unit = Obj.magic _4 in
          let _3 : Syntax.myfactor = Obj.magic _3 in
          let _2 : unit list = Obj.magic _2 in
          let _1 : unit = Obj.magic _1 in
          let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
          let _startpos = _startpos__1_ in
          let _endpos = _endpos__4_ in
          let _v : Syntax.myfactor =
            let _1 =
              let _1 =
                print_endline (Batteries.dump ("DEBUG:rhs", _3)); NFactor _3 in
              print_endline (Batteries.dump ("DEBUG:cterm/group", _1));
              NFactor _1 in
            print_endline (Batteries.dump ("DEBUG:term/cterms", _1));
            NFactor _1 in
          {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = (Obj.repr _v);
            MenhirLib.EngineTypes.startp = _startpos;
            MenhirLib.EngineTypes.endp = _endpos;
            MenhirLib.EngineTypes.next = _menhir_stack
          }));((fun _menhir_env ->
                  let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
                  let { MenhirLib.EngineTypes.state = _menhir_s;
                        MenhirLib.EngineTypes.semv = _1;
                        MenhirLib.EngineTypes.startp = _startpos__1_;
                        MenhirLib.EngineTypes.endp = _endpos__1_;
                        MenhirLib.EngineTypes.next = _menhir_stack }
                    = _menhir_stack in
                  let _1 : Syntax.myfactor = Obj.magic _1 in
                  let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
                  let _startpos = _startpos__1_ in
                  let _endpos = _endpos__1_ in
                  let _v : Syntax.myfactor =
                    let _1 =
                      let _1 =
                        print_endline (Batteries.dump ("DEBUG:class1a", _1));
                        NFactor _1 in
                      print_endline
                        (Batteries.dump ("DEBUG:cterm/class", _1));
                      NFactor _1 in
                    print_endline (Batteries.dump ("DEBUG:term/cterms", _1));
                    NFactor _1 in
                  {
                    MenhirLib.EngineTypes.state = _menhir_s;
                    MenhirLib.EngineTypes.semv = (Obj.repr _v);
                    MenhirLib.EngineTypes.startp = _startpos;
                    MenhirLib.EngineTypes.endp = _endpos;
                    MenhirLib.EngineTypes.next = _menhir_stack
                  }));((fun _menhir_env ->
                          let _menhir_stack =
                            _menhir_env.MenhirLib.EngineTypes.stack in
                          let { MenhirLib.EngineTypes.state = _menhir_s;
                                MenhirLib.EngineTypes.semv = _1;
                                MenhirLib.EngineTypes.startp = _startpos__1_;
                                MenhirLib.EngineTypes.endp = _endpos__1_;
                                MenhirLib.EngineTypes.next = _menhir_stack }
                            = _menhir_stack in
                          let _1 : string Positions.located = Obj.magic _1 in
                          let _endpos__0_ =
                            _menhir_stack.MenhirLib.EngineTypes.endp in
                          let _startpos = _startpos__1_ in
                          let _endpos = _endpos__1_ in
                          let _v : Syntax.myfactor =
                            let _1 =
                              let _1 =
                                print_endline
                                  (Batteries.dump ("DEBUG:class", _1));
                                SFactor _1 in
                              print_endline
                                (Batteries.dump ("DEBUG:cterm/class", _1));
                              NFactor _1 in
                            print_endline
                              (Batteries.dump ("DEBUG:term/cterms", _1));
                            NFactor _1 in
                          {
                            MenhirLib.EngineTypes.state = _menhir_s;
                            MenhirLib.EngineTypes.semv = (Obj.repr _v);
                            MenhirLib.EngineTypes.startp = _startpos;
                            MenhirLib.EngineTypes.endp = _endpos;
                            MenhirLib.EngineTypes.next = _menhir_stack
                          }));((fun _menhir_env ->
                                  let _menhir_stack =
                                    _menhir_env.MenhirLib.EngineTypes.stack in
                                  let {
                                        MenhirLib.EngineTypes.state =
                                          _menhir_s;
                                        MenhirLib.EngineTypes.semv = _1;
                                        MenhirLib.EngineTypes.startp =
                                          _startpos__1_;
                                        MenhirLib.EngineTypes.endp =
                                          _endpos__1_;
                                        MenhirLib.EngineTypes.next =
                                          _menhir_stack
                                        }
                                    = _menhir_stack in
                                  let _1 : string Positions.located =
                                    Obj.magic _1 in
                                  let _endpos__0_ =
                                    _menhir_stack.MenhirLib.EngineTypes.endp in
                                  let _startpos = _startpos__1_ in
                                  let _endpos = _endpos__1_ in
                                  let _v : Syntax.myfactor =
                                    let _1 =
                                      let _1 =
                                        print_endline
                                          (Batteries.dump ("DEBUG:quid", _1));
                                        _1 in
                                      print_endline
                                        (Batteries.dump
                                           ("DEBUG:sterm/quid", _1));
                                      SFactor _1 in
                                    print_endline
                                      (Batteries.dump
                                         ("DEBUG:term/sterm", _1));
                                    NFactor _1 in
                                  {
                                    MenhirLib.EngineTypes.state = _menhir_s;
                                    MenhirLib.EngineTypes.semv =
                                      (Obj.repr _v);
                                    MenhirLib.EngineTypes.startp = _startpos;
                                    MenhirLib.EngineTypes.endp = _endpos;
                                    MenhirLib.EngineTypes.next =
                                      _menhir_stack
                                  }));((fun _menhir_env ->
                                          let _menhir_stack =
                                            _menhir_env.MenhirLib.EngineTypes.stack in
                                          let {
                                                MenhirLib.EngineTypes.state =
                                                  _menhir_s;
                                                MenhirLib.EngineTypes.semv =
                                                  _1;
                                                MenhirLib.EngineTypes.startp
                                                  = _startpos__1_;
                                                MenhirLib.EngineTypes.endp =
                                                  _endpos__1_;
                                                MenhirLib.EngineTypes.next =
                                                  _menhir_stack
                                                }
                                            = _menhir_stack in
                                          let _1 : string Positions.located =
                                            Obj.magic _1 in
                                          let _endpos__0_ =
                                            _menhir_stack.MenhirLib.EngineTypes.endp in
                                          let _startpos = _startpos__1_ in
                                          let _endpos = _endpos__1_ in
                                          let _v : Syntax.myfactor =
                                            let _1 =
                                              let _1 =
                                                print_endline
                                                  (Batteries.dump
                                                     ("DEBUG:lid", _1));
                                                _1 in
                                              print_endline
                                                (Batteries.dump
                                                   ("DEBUG:sterm/lid", _1));
                                              SFactor _1 in
                                            print_endline
                                              (Batteries.dump
                                                 ("DEBUG:term/sterm", _1));
                                            NFactor _1 in
                                          {
                                            MenhirLib.EngineTypes.state =
                                              _menhir_s;
                                            MenhirLib.EngineTypes.semv =
                                              (Obj.repr _v);
                                            MenhirLib.EngineTypes.startp =
                                              _startpos;
                                            MenhirLib.EngineTypes.endp =
                                              _endpos;
                                            MenhirLib.EngineTypes.next =
                                              _menhir_stack
                                          }))|]
    and trace =
      Some
        ([|"error";"Tchar";"STAR";"RPAREN";"REGEX";"QUESTION";"QID";"PLUS";"NEWLINE";"LPAREN";"LID";"EOF";"DASH";"COLONCOLONEQUAL";"CARET";"BAR";"#"|],
          [|"Accepting";"Reducing production alternation -> alternation BAR list(NEWLINE) concatenation";"Reducing production alternation -> concatenation";"Reducing production char_class -> CARET char_class1";"Reducing production char_class -> char_class1";"Reducing production char_class1 -> Tchar DASH Tchar";"Reducing production char_class1 -> char_class1 Tchar";"Reducing production char_class1 -> Tchar";"Reducing production concatenation -> concatenation factor";"Reducing production concatenation -> factor";"Reducing production factor -> term PLUS";"Reducing production factor -> term QUESTION";"Reducing production factor -> term STAR";"Reducing production factor -> term";"Reducing production grammar -> rules postlude";"Reducing production list(NEWLINE) ->";"Reducing production list(NEWLINE) -> NEWLINE list(NEWLINE)";"Reducing production nonempty_list(NEWLINE) -> NEWLINE";"Reducing production nonempty_list(NEWLINE) -> NEWLINE nonempty_list(NEWLINE)";"Reducing production postlude -> list(NEWLINE) EOF";"Reducing production rhs -> alternation";"Reducing production rule -> LID COLONCOLONEQUAL rhs";"Reducing production rules -> rules nonempty_list(NEWLINE) rule";"Reducing production rules -> nonempty_list(NEWLINE) rule";"Reducing production rules -> rule";"Reducing production term -> LPAREN list(NEWLINE) rhs RPAREN";"Reducing production term -> char_class";"Reducing production term -> REGEX";"Reducing production term -> QID";"Reducing production term -> LID"|])
  end
module MenhirInterpreter =
  struct
    module ET = (MenhirLib.TableInterpreter.MakeEngineTable)(Tables)
    module TI = (MenhirLib.Engine.Make)(ET)
    include TI
  end
let grammar lexer lexbuf =
  (Obj.magic (MenhirInterpreter.entry `Legacy 0 lexer lexbuf) : Syntax.partial_grammar)
module Incremental =
  struct
    let grammar initial_position =
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : Syntax.partial_grammar
                                                                  MenhirInterpreter.checkpoint)
  end
let () = Ppx_inline_test_lib.unset_lib "gbnf_parser"
let () = Expect_test_collector.Current_file.unset ()
let () = Ppx_bench_lib.Benchmark_accumulator.Current_libname.unset ()
let () =
  Ppx_module_timer_runtime.record_until Ppx_module_timer_runtime.__MODULE__

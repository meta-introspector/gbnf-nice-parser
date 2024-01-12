
open Parsetree
let f = [{pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat =
       {ppat_desc = Ppat_construct ({txt = Lident "()"}, None);
        ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_ident
          {txt =
            Ldot (Ldot (Lident "MenhirLib", "StaticVersion"),
             "require_20230608")};
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_module
    {pmb_name = {txt = Some "MenhirBasics"};
     pmb_expr =
      {pmod_desc =
        Pmod_structure
         [{pstr_desc =
            Pstr_exception
             {ptyexn_constructor =
               {pext_name = {txt = "Error"};
                pext_kind = Pext_decl (Pcstr_tuple [], None)}}};
          {pstr_desc =
            Pstr_value (Nonrecursive,
             [{pvb_pat =
                {ppat_desc = Ppat_var {txt = "_eRR"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_fun (Nolabel, None,
                   {ppat_desc = Ppat_var {txt = "_s"}; ppat_loc_stack = []},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc = Pexp_ident {txt = Lident "raise"};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc =
                          Pexp_construct ({txt = Lident "Error"}, None);
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                 pexp_loc_stack = []}}])};
          {pstr_desc =
            Pstr_type (Recursive,
             [{ptype_name = {txt = "token"}; ptype_params = [];
               ptype_cstrs = [];
               ptype_kind =
                Ptype_variant
                 [{pcd_name = {txt = "Tchar"};
                   pcd_args =
                    Pcstr_tuple
                     [{ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
                       ptyp_loc_stack = []}];
                   pcd_res = None};
                  {pcd_name = {txt = "STAR"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "RPAREN"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "REGEX"};
                   pcd_args =
                    Pcstr_tuple
                     [{ptyp_desc =
                        Ptyp_constr
                         ({txt = Ldot (Lident "Positions", "located")},
                         [{ptyp_desc =
                            Ptyp_constr ({txt = Lident "string"}, []);
                           ptyp_loc_stack = []}]);
                       ptyp_loc_stack = []}];
                   pcd_res = None};
                  {pcd_name = {txt = "QUESTION"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "QID"};
                   pcd_args =
                    Pcstr_tuple
                     [{ptyp_desc =
                        Ptyp_constr
                         ({txt = Ldot (Lident "Positions", "located")},
                         [{ptyp_desc =
                            Ptyp_constr ({txt = Lident "string"}, []);
                           ptyp_loc_stack = []}]);
                       ptyp_loc_stack = []}];
                   pcd_res = None};
                  {pcd_name = {txt = "PLUS"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "NEWLINE"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "LPAREN"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "LID"};
                   pcd_args =
                    Pcstr_tuple
                     [{ptyp_desc =
                        Ptyp_constr
                         ({txt = Ldot (Lident "Positions", "located")},
                         [{ptyp_desc =
                            Ptyp_constr ({txt = Lident "string"}, []);
                           ptyp_loc_stack = []}]);
                       ptyp_loc_stack = []}];
                   pcd_res = None};
                  {pcd_name = {txt = "EOF"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "DASH"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "COLONCOLONEQUAL"};
                   pcd_args = Pcstr_tuple []; pcd_res = None};
                  {pcd_name = {txt = "CARET"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None};
                  {pcd_name = {txt = "BAR"}; pcd_args = Pcstr_tuple [];
                   pcd_res = None}];
               ptype_private = Public; ptype_manifest = None}])}]}}};
 {pstr_desc =
   Pstr_include
    {pincl_mod = {pmod_desc = Pmod_ident {txt = Lident "MenhirBasics"}}}};
 {pstr_desc =
   Pstr_open
    {popen_expr = {pmod_desc = Pmod_ident {txt = Lident "Syntax"}};
     popen_override = Fresh}};
 {pstr_desc =
   Pstr_module
    {pmb_name = {txt = Some "Tables"};
     pmb_expr =
      {pmod_desc =
        Pmod_structure
         [{pstr_desc =
            Pstr_include
             {pincl_mod =
               {pmod_desc = Pmod_ident {txt = Lident "MenhirBasics"}}}};
          {pstr_desc =
            Pstr_value (Nonrecursive,
             [{pvb_pat =
                {ppat_desc =
                  Ppat_constraint
                   ({ppat_desc = Ppat_var {txt = "token2terminal"};
                     ppat_loc_stack = []},
                   {ptyp_desc =
                     Ptyp_poly ([],
                      {ptyp_desc =
                        Ptyp_arrow (Nolabel,
                         {ptyp_desc =
                           Ptyp_constr ({txt = Lident "token"}, []);
                          ptyp_loc_stack = []},
                         {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
                          ptyp_loc_stack = []});
                       ptyp_loc_stack = []});
                    ptyp_loc_stack = []});
                 ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_constraint
                   ({pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_tok"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_match
                          ({pexp_desc = Pexp_ident {txt = Lident "_tok"};
                            pexp_loc_stack = []},
                          [{pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "BAR"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("15", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "CARET"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("14", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct
                                ({txt = Lident "COLONCOLONEQUAL"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("13", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "DASH"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("12", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "EOF"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("11", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "LID"},
                                Some
                                 {ppat_desc = Ppat_any; ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("10", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "LPAREN"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("9", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "NEWLINE"},
                                None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("8", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "PLUS"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("7", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "QID"},
                                Some
                                 {ppat_desc = Ppat_any; ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("6", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "QUESTION"},
                                None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("5", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "REGEX"},
                                Some
                                 {ppat_desc = Ppat_any; ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("4", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "RPAREN"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("3", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "STAR"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("2", None));
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "Tchar"},
                                Some
                                 {ppat_desc = Ppat_any; ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_constant (Pconst_integer ("1", None));
                              pexp_loc_stack = []}}]);
                        pexp_loc_stack = []});
                     pexp_loc_stack = []},
                   {ptyp_desc =
                     Ptyp_arrow (Nolabel,
                      {ptyp_desc = Ptyp_constr ({txt = Lident "token"}, []);
                       ptyp_loc_stack = []},
                      {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
                       ptyp_loc_stack = []});
                    ptyp_loc_stack = []});
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "error_terminal"};
                 ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc = Pexp_constant (Pconst_integer ("0", None));
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc =
                  Ppat_constraint
                   ({ppat_desc = Ppat_var {txt = "token2value"};
                     ppat_loc_stack = []},
                   {ptyp_desc =
                     Ptyp_poly ([],
                      {ptyp_desc =
                        Ptyp_arrow (Nolabel,
                         {ptyp_desc =
                           Ptyp_constr ({txt = Lident "token"}, []);
                          ptyp_loc_stack = []},
                         {ptyp_desc =
                           Ptyp_constr ({txt = Ldot (Lident "Obj", "t")}, []);
                          ptyp_loc_stack = []});
                       ptyp_loc_stack = []});
                    ptyp_loc_stack = []});
                 ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_constraint
                   ({pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_tok"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_match
                          ({pexp_desc = Pexp_ident {txt = Lident "_tok"};
                            pexp_loc_stack = []},
                          [{pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "BAR"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "CARET"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct
                                ({txt = Lident "COLONCOLONEQUAL"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "DASH"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "EOF"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "LID"},
                                Some
                                 {ppat_desc = Ppat_var {txt = "_v"};
                                  ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc = Pexp_ident {txt = Lident "_v"};
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "LPAREN"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "NEWLINE"},
                                None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "PLUS"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "QID"},
                                Some
                                 {ppat_desc = Ppat_var {txt = "_v"};
                                  ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc = Pexp_ident {txt = Lident "_v"};
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "QUESTION"},
                                None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "REGEX"},
                                Some
                                 {ppat_desc = Ppat_var {txt = "_v"};
                                  ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc = Pexp_ident {txt = Lident "_v"};
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "RPAREN"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "STAR"}, None);
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "()"},
                                     None);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}};
                           {pc_lhs =
                             {ppat_desc =
                               Ppat_construct ({txt = Lident "Tchar"},
                                Some
                                 {ppat_desc = Ppat_var {txt = "_v"};
                                  ppat_loc_stack = []});
                              ppat_loc_stack = []};
                            pc_guard = None;
                            pc_rhs =
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Obj", "repr")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc = Pexp_ident {txt = Lident "_v"};
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []}}]);
                        pexp_loc_stack = []});
                     pexp_loc_stack = []},
                   {ptyp_desc =
                     Ptyp_arrow (Nolabel,
                      {ptyp_desc = Ptyp_constr ({txt = Lident "token"}, []);
                       ptyp_loc_stack = []},
                      {ptyp_desc =
                        Ptyp_constr ({txt = Ldot (Lident "Obj", "t")}, []);
                       ptyp_loc_stack = []});
                    ptyp_loc_stack = []});
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "default_reduction"};
                 ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_tuple
                   [{pexp_desc = Pexp_constant (Pconst_integer ("8", None));
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_constant
                       (Pconst_string
                         ("\000\000\019\000\000\000\000\006\028\029\000\000\017\000\030\000\000\007\000\r\012\011\000\026\n\000\t\000\027\000\000\000\000\022\000\000\015\000\023\000\020\025\000\024\001",
                         None));
                     pexp_loc_stack = []}];
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "error"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_tuple
                   [{pexp_desc = Pexp_constant (Pconst_integer ("16", None));
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_constant
                       (Pconst_string
                         ("\000 \000 \000\000\000\004Jb\127û@\000\000\000\000\000\000\000JâJâ\000\000Jb\000\000@\000\127ó\000\000\127ó\000\000\000\000\000\000\016\000\000\000\000\000Zó\000\000\127ó\000\000\016‘JâJbZó\000\000\000\000°\000\000\000 \000\000\000\016\000\000\000\000\000 \000\000\000\000",
                         None));
                     pexp_loc_stack = []}];
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "start"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc = Pexp_constant (Pconst_integer ("1", None));
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "action"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_tuple
                   [{pexp_desc =
                      Pexp_tuple
                       [{pexp_desc =
                          Pexp_constant (Pconst_integer ("8", None));
                         pexp_loc_stack = []};
                        {pexp_desc =
                          Pexp_constant
                           (Pconst_string
                             ("pŽ\000\003v\0032\000\000\000¬¬\000v\0004\028\000:\000\000\000L\000\000v\000X\000´¬v”\000²²\000@\000Z\000\000@\000\000",
                             None));
                         pexp_loc_stack = []}];
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_tuple
                       [{pexp_desc =
                          Pexp_constant (Pconst_integer ("8", None));
                         pexp_loc_stack = []};
                        {pexp_desc =
                          Pexp_constant
                           (Pconst_string
                             ("\029\029\029\029\029\029\029\029\029\029\029\026\018\029\029F\r\r\r\r\r\r\r\r\r\r\030\022\r\r5N55R5V5555^\01455F\017\017\017\017\017\017\017\017\017\017£\000\017\017\022\000\t\"\006&\014\t*:\t\000\000>\t\022\000\005\"\006&E\005*:\005\000=>\005=\000=Q.==ŽQE=Q\000\000\000z",
                             None));
                         pexp_loc_stack = []}];
                     pexp_loc_stack = []}];
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "lhs"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_tuple
                   [{pexp_desc = Pexp_constant (Pconst_integer ("4", None));
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_constant
                       (Pconst_string ("\rÜË»ª™™‡veC\"!\017\017", None));
                     pexp_loc_stack = []}];
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "goto"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_tuple
                   [{pexp_desc =
                      Pexp_tuple
                       [{pexp_desc =
                          Pexp_constant (Pconst_integer ("8", None));
                         pexp_loc_stack = []};
                        {pexp_desc =
                          Pexp_constant
                           (Pconst_string
                             ("\003\005\000\000\003\000\000\000\000\000\003\016\000\024\000\012\000\000\000\000\000\000\000\000\000:\000\000\000\000\024\":\0004\016\000\"\000\000\000\000>\000\000",
                             None));
                         pexp_loc_stack = []}];
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_tuple
                       [{pexp_desc =
                          Pexp_constant (Pconst_integer ("8", None));
                         pexp_loc_stack = []};
                        {pexp_desc =
                          Pexp_constant
                           (Pconst_string
                             ("\019#*\"\003+\014-\025\026\028\029\030\019\003\r\023\017\019 '\025\026\028\029\030\025!\028\029\019%&(,\000\000\000\027\000\028\029",
                             None));
                         pexp_loc_stack = []}];
                     pexp_loc_stack = []}];
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "semantic_action"};
                 ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_array
                   [{pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_4"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__4_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__4_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc = Ppat_any;
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_3"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__3_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__3_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {ppat_desc = Ppat_any;
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_2"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_startpos__2_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos__2_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {ppat_desc =
                                                    Ppat_record
                                                     ([({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "state")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_menhir_s"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "semv")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_1"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "startp")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt =
                                                             "_startpos__1_"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "endp")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt =
                                                             "_endpos__1_"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "next")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt =
                                                             "_menhir_stack"};
                                                         ppat_loc_stack = []})],
                                                     Closed);
                                                   ppat_loc_stack = []})],
                                               Closed);
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_4"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_4"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_3"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "list"},
                                               [{ptyp_desc =
                                                  Ptyp_constr
                                                   ({txt = Lident "unit"},
                                                   []);
                                                 ptyp_loc_stack = []}]);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_3"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "list"},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "unit"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_constraint
                                            ({ppat_desc =
                                               Ppat_var {txt = "_2"};
                                              ppat_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_poly ([],
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt = Lident "unit"}, 
                                                  []);
                                                ptyp_loc_stack = []});
                                             ptyp_loc_stack = []});
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_constraint
                                            ({pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt =
                                                      Ldot (Lident "Obj",
                                                       "magic")};
                                                  pexp_loc_stack = []},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_2"};
                                                   pexp_loc_stack = []})]);
                                              pexp_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "unit"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_constraint
                                               ({ppat_desc =
                                                  Ppat_var {txt = "_1"};
                                                 ppat_loc_stack = []},
                                               {ptyp_desc =
                                                 Ptyp_poly ([],
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                ptyp_loc_stack = []});
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_constraint
                                               ({pexp_desc =
                                                  Pexp_apply
                                                   ({pexp_desc =
                                                      Pexp_ident
                                                       {txt =
                                                         Ldot (Lident "Obj",
                                                          "magic")};
                                                     pexp_loc_stack = []},
                                                   [(Nolabel,
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt = Lident "_1"};
                                                      pexp_loc_stack = []})]);
                                                 pexp_loc_stack = []},
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt =
                                                     Ldot (Lident "Syntax",
                                                      "myfactor")},
                                                  []);
                                                ptyp_loc_stack = []});
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var
                                                  {txt = "_endpos__0_"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_field
                                                  ({pexp_desc =
                                                     Pexp_ident
                                                      {txt =
                                                        Lident
                                                         "_menhir_stack"};
                                                    pexp_loc_stack = []},
                                                  {txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_startpos"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos__1_"};
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var
                                                        {txt = "_endpos"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_endpos__4_"};
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_let (Nonrecursive,
                                                     [{pvb_pat =
                                                        {ppat_desc =
                                                          Ppat_constraint
                                                           ({ppat_desc =
                                                              Ppat_var
                                                               {txt = "_v"};
                                                             ppat_loc_stack =
                                                              []},
                                                           {ptyp_desc =
                                                             Ptyp_poly (
                                                              [],
                                                              {ptyp_desc =
                                                                Ptyp_constr
                                                                 ({txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Syntax",
                                                                    "myfactor")},
                                                                 []);
                                                               ptyp_loc_stack
                                                                = []});
                                                            ptyp_loc_stack =
                                                             []});
                                                         ppat_loc_stack = []};
                                                       pvb_expr =
                                                        {pexp_desc =
                                                          Pexp_constraint
                                                           ({pexp_desc =
                                                              Pexp_construct
                                                               ({txt =
                                                                  Lident
                                                                   "NFactor"},
                                                               Some
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_1"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                             pexp_loc_stack =
                                                              []},
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "myfactor")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []}}],
                                                     {pexp_desc =
                                                       Pexp_record
                                                        ([({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "state")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_menhir_s"};
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "semv")},
                                                           {pexp_desc =
                                                             Pexp_apply
                                                              ({pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Obj",
                                                                    "repr")};
                                                                pexp_loc_stack
                                                                 = []},
                                                              [(Nolabel,
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_v"};
                                                                 pexp_loc_stack
                                                                  = []})]);
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "startp")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_startpos"};
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "endp")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_endpos"};
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "next")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_menhir_stack"};
                                                            pexp_loc_stack =
                                                             []})],
                                                        None);
                                                      pexp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_sequence
                                                      ({pexp_desc =
                                                         Pexp_apply
                                                          ({pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "print_endline"};
                                                            pexp_loc_stack =
                                                             []},
                                                          [(Nolabel,
                                                            {pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:alt",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []})]);
                                                        pexp_loc_stack =
                                                         []},
                                                      {pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident "NFactor"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_1"};
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_2"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_2"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_2"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "unit"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos__2_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "myfactor")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:ccrs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_2"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_2"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_sequence
                                                      ({pexp_desc =
                                                         Pexp_apply
                                                          ({pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "print_endline"};
                                                            pexp_loc_stack =
                                                             []},
                                                          [(Nolabel,
                                                            {pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cc2rs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []})]);
                                                        pexp_loc_stack =
                                                         []},
                                                      {pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident
                                                             "CharClass"},
                                                         None);
                                                       pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_3"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__3_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__3_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc = Ppat_any;
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_2"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__2_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__2_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_menhir_s"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_1"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_startpos__1_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos__1_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_menhir_stack"};
                                                   ppat_loc_stack = []})],
                                               Closed);
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_3"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr ({txt = Lident "int"},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_3"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "int"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_2"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "unit"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_2"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_constraint
                                            ({ppat_desc =
                                               Ppat_var {txt = "_1"};
                                              ppat_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_poly ([],
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt = Lident "int"}, 
                                                  []);
                                                ptyp_loc_stack = []});
                                             ptyp_loc_stack = []});
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_constraint
                                            ({pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt =
                                                      Ldot (Lident "Obj",
                                                       "magic")};
                                                  pexp_loc_stack = []},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_1"};
                                                   pexp_loc_stack = []})]);
                                              pexp_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "int"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__0_"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_field
                                               ({pexp_desc =
                                                  Pexp_ident
                                                   {txt =
                                                     Lident "_menhir_stack"};
                                                 pexp_loc_stack = []},
                                               {txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "endp")});
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_startpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident "_startpos__1_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_endpos__3_"};
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_constraint
                                                        ({ppat_desc =
                                                           Ppat_var
                                                            {txt = "_v"};
                                                          ppat_loc_stack = []},
                                                        {ptyp_desc =
                                                          Ptyp_poly (
                                                           [],
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "myfactor")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         ptyp_loc_stack = []});
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_constraint
                                                        ({pexp_desc =
                                                           Pexp_sequence
                                                            ({pexp_desc =
                                                               Pexp_apply
                                                                ({pexp_desc =
                                                                   Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []},
                                                                [(Nolabel,
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cc3rs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_2"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                              pexp_loc_stack
                                                               = []},
                                                            {pexp_desc =
                                                              Pexp_construct
                                                               ({txt =
                                                                  Lident
                                                                   "CharInt"},
                                                               Some
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_1"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                             pexp_loc_stack =
                                                              []});
                                                          pexp_loc_stack =
                                                           []},
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "myfactor")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_record
                                                     ([({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "state")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_menhir_s"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "semv")},
                                                        {pexp_desc =
                                                          Pexp_apply
                                                           ({pexp_desc =
                                                              Pexp_ident
                                                               {txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Obj",
                                                                  "repr")};
                                                             pexp_loc_stack =
                                                              []},
                                                           [(Nolabel,
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_v"};
                                                              pexp_loc_stack
                                                               = []})]);
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "startp")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_startpos"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "endp")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident "_endpos"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "next")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_menhir_stack"};
                                                         pexp_loc_stack = []})],
                                                     None);
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_2"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_2"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr ({txt = Lident "int"},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_2"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "int"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos__2_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "myfactor")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cc4rs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_1"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr ({txt = Lident "int"},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "int"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_sequence
                                                      ({pexp_desc =
                                                         Pexp_apply
                                                          ({pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "print_endline"};
                                                            pexp_loc_stack =
                                                             []},
                                                          [(Nolabel,
                                                            {pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cc5rs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []})]);
                                                        pexp_loc_stack =
                                                         []},
                                                      {pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident "CharInt"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_1"};
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_2"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_2"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_2"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos__2_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "myfactor")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:concat1",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_1"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_sequence
                                                      ({pexp_desc =
                                                         Pexp_apply
                                                          ({pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "print_endline"};
                                                            pexp_loc_stack =
                                                             []},
                                                          [(Nolabel,
                                                            {pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:concat2",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []})]);
                                                        pexp_loc_stack =
                                                         []},
                                                      {pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident "NFactor"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_1"};
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_1_inlined1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_startpos__1_inlined1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_endpos__1_inlined1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc =
                                         Ppat_var {txt = "_1_inlined1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_1_inlined1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident
                                                     "_endpos__1_inlined1_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_v"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_let (Nonrecursive,
                                                     [{pvb_pat =
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_2"};
                                                         ppat_loc_stack = []};
                                                       pvb_expr =
                                                        {pexp_desc =
                                                          Pexp_let
                                                           (Nonrecursive,
                                                           [{pvb_pat =
                                                              {ppat_desc =
                                                                Ppat_var
                                                                 {txt = "_1"};
                                                               ppat_loc_stack
                                                                = []};
                                                             pvb_expr =
                                                              {pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "_1_inlined1"};
                                                               pexp_loc_stack
                                                                = []}}],
                                                           {pexp_desc =
                                                             Pexp_let
                                                              (Nonrecursive,
                                                              [{pvb_pat =
                                                                 {ppat_desc =
                                                                   Ppat_var
                                                                    {txt =
                                                                    "_1"};
                                                                  ppat_loc_stack
                                                                   = 
                                                                   []};
                                                                pvb_expr =
                                                                 {pexp_desc =
                                                                   Pexp_sequence
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:plus",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "Plus"},
                                                                    None);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []}}],
                                                              {pexp_desc =
                                                                Pexp_sequence
                                                                 ({pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:mod",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                 {pexp_desc =
                                                                   Pexp_construct
                                                                    (
                                                                    {txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []}}],
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:termfactor",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                 Some
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_constraint
                                                           ({pexp_desc =
                                                              Pexp_construct
                                                               ({txt =
                                                                  Lident
                                                                   "NFactor"},
                                                               Some
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_1"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                             pexp_loc_stack =
                                                              []},
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "myfactor")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []});
                                                      pexp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_1_inlined1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_startpos__1_inlined1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_endpos__1_inlined1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc =
                                         Ppat_var {txt = "_1_inlined1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_1_inlined1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident
                                                     "_endpos__1_inlined1_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_v"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_let (Nonrecursive,
                                                     [{pvb_pat =
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_2"};
                                                         ppat_loc_stack = []};
                                                       pvb_expr =
                                                        {pexp_desc =
                                                          Pexp_let
                                                           (Nonrecursive,
                                                           [{pvb_pat =
                                                              {ppat_desc =
                                                                Ppat_var
                                                                 {txt = "_1"};
                                                               ppat_loc_stack
                                                                = []};
                                                             pvb_expr =
                                                              {pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "_1_inlined1"};
                                                               pexp_loc_stack
                                                                = []}}],
                                                           {pexp_desc =
                                                             Pexp_let
                                                              (Nonrecursive,
                                                              [{pvb_pat =
                                                                 {ppat_desc =
                                                                   Ppat_var
                                                                    {txt =
                                                                    "_1"};
                                                                  ppat_loc_stack
                                                                   = 
                                                                   []};
                                                                pvb_expr =
                                                                 {pexp_desc =
                                                                   Pexp_sequence
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:quest",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "Question"},
                                                                    None);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []}}],
                                                              {pexp_desc =
                                                                Pexp_sequence
                                                                 ({pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:quest",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                 {pexp_desc =
                                                                   Pexp_construct
                                                                    (
                                                                    {txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []}}],
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:termfactor",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                 Some
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_constraint
                                                           ({pexp_desc =
                                                              Pexp_construct
                                                               ({txt =
                                                                  Lident
                                                                   "NFactor"},
                                                               Some
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_1"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                             pexp_loc_stack =
                                                              []},
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "myfactor")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []});
                                                      pexp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_1_inlined1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_startpos__1_inlined1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_endpos__1_inlined1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc =
                                         Ppat_var {txt = "_1_inlined1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_1_inlined1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident
                                                     "_endpos__1_inlined1_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_v"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_let (Nonrecursive,
                                                     [{pvb_pat =
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_2"};
                                                         ppat_loc_stack = []};
                                                       pvb_expr =
                                                        {pexp_desc =
                                                          Pexp_let
                                                           (Nonrecursive,
                                                           [{pvb_pat =
                                                              {ppat_desc =
                                                                Ppat_var
                                                                 {txt = "_1"};
                                                               ppat_loc_stack
                                                                = []};
                                                             pvb_expr =
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "Star"},
                                                                 None);
                                                               pexp_loc_stack
                                                                = []}}],
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:star",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                 Some
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []}}],
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:termfactor",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                 Some
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_constraint
                                                           ({pexp_desc =
                                                              Pexp_construct
                                                               ({txt =
                                                                  Lident
                                                                   "NFactor"},
                                                               Some
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_1"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                             pexp_loc_stack =
                                                              []},
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "myfactor")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []});
                                                      pexp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_v"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var {txt = "_1"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_sequence
                                                        ({pexp_desc =
                                                           Pexp_apply
                                                            ({pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident
                                                                   "print_endline"};
                                                              pexp_loc_stack
                                                               = []},
                                                            [(Nolabel,
                                                              {pexp_desc =
                                                                Pexp_apply
                                                                 ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                 [(Nolabel,
                                                                   {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:termfactor",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                               pexp_loc_stack
                                                                = []})]);
                                                          pexp_loc_stack =
                                                           []},
                                                        {pexp_desc =
                                                          Pexp_construct
                                                           ({txt =
                                                              Lident
                                                               "NFactor"},
                                                           Some
                                                            {pexp_desc =
                                                              Pexp_ident
                                                               {txt =
                                                                 Lident "_1"};
                                                             pexp_loc_stack =
                                                              []});
                                                         pexp_loc_stack = []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident "NFactor"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_1"};
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_2"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "rs"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos_rs_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos_rs_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_2"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_2"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "rs"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "rs"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos_rs_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos__2_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "partial_grammar")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:grammar",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "rs"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_2"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_record
                                                            ([({txt =
                                                                 Lident
                                                                  "pg_filename"},
                                                               {pexp_desc =
                                                                 Pexp_constant
                                                                  (Pconst_string
                                                                    ("",
                                                                    None));
                                                                pexp_loc_stack
                                                                 = []});
                                                              ({txt =
                                                                 Lident
                                                                  "pg_rules"},
                                                               {pexp_desc =
                                                                 Pexp_construct
                                                                  ({txt =
                                                                    Lident
                                                                    "[]"},
                                                                  None);
                                                                pexp_loc_stack
                                                                 = []})],
                                                            None);
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "partial_grammar")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc = Ppat_var {txt = "_menhir_s"};
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_field
                                   ({pexp_desc =
                                      Pexp_ident {txt = Lident "_menhir_env"};
                                     pexp_loc_stack = []},
                                   {txt =
                                     Ldot
                                      (Ldot (Lident "MenhirLib",
                                        "EngineTypes"),
                                      "current")});
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_var {txt = "_endpos__0_"};
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_field
                                      ({pexp_desc =
                                         Pexp_ident
                                          {txt = Lident "_menhir_stack"};
                                        pexp_loc_stack = []},
                                      {txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_constraint
                                               ({ppat_desc =
                                                  Ppat_var {txt = "_v"};
                                                 ppat_loc_stack = []},
                                               {ptyp_desc =
                                                 Ptyp_poly ([],
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt = Lident "list"},
                                                     [{ptyp_desc =
                                                        Ptyp_constr
                                                         ({txt =
                                                            Lident "unit"},
                                                         []);
                                                       ptyp_loc_stack = []}]);
                                                   ptyp_loc_stack = []});
                                                ptyp_loc_stack = []});
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_constraint
                                               ({pexp_desc =
                                                  Pexp_construct
                                                   ({txt = Lident "[]"},
                                                   None);
                                                 pexp_loc_stack = []},
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt = Lident "list"},
                                                  [{ptyp_desc =
                                                     Ptyp_constr
                                                      ({txt = Lident "unit"},
                                                      []);
                                                    ptyp_loc_stack = []}]);
                                                ptyp_loc_stack = []});
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_record
                                            ([({txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "state")},
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_menhir_s"};
                                                pexp_loc_stack = []});
                                              ({txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "semv")},
                                               {pexp_desc =
                                                 Pexp_apply
                                                  ({pexp_desc =
                                                     Pexp_ident
                                                      {txt =
                                                        Ldot (Lident "Obj",
                                                         "repr")};
                                                    pexp_loc_stack = []},
                                                  [(Nolabel,
                                                    {pexp_desc =
                                                      Pexp_ident
                                                       {txt = Lident "_v"};
                                                     pexp_loc_stack = []})]);
                                                pexp_loc_stack = []});
                                              ({txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "startp")},
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_startpos"};
                                                pexp_loc_stack = []});
                                              ({txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "endp")},
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos"};
                                                pexp_loc_stack = []});
                                              ({txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "next")},
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident "_menhir_stack"};
                                                pexp_loc_stack = []})],
                                            None);
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "xs"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos_xs_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos_xs_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc = Ppat_var {txt = "x"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos_x_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos_x_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "xs"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "list"},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "unit"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "xs"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "list"},
                                         [{ptyp_desc =
                                            Ptyp_constr
                                             ({txt = Lident "unit"}, 
                                             []);
                                           ptyp_loc_stack = []}]);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "x"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "unit"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "x"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos_x_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos_xs_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Lident "list"},
                                                           [{ptyp_desc =
                                                              Ptyp_constr
                                                               ({txt =
                                                                  Lident
                                                                   "unit"},
                                                               []);
                                                             ptyp_loc_stack =
                                                              []}]);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_construct
                                                         ({txt = Lident "::"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_tuple
                                                             [{pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident "x"};
                                                               pexp_loc_stack
                                                                = []};
                                                              {pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "xs"};
                                                               pexp_loc_stack
                                                                = []}];
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt = Lident "list"},
                                                        [{ptyp_desc =
                                                           Ptyp_constr
                                                            ({txt =
                                                               Lident "unit"},
                                                            []);
                                                          ptyp_loc_stack = []}]);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "x"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos_x_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos_x_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "x"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "x"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos_x_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos_x_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt = Lident "list"},
                                                        [{ptyp_desc =
                                                           Ptyp_constr
                                                            ({txt =
                                                               Lident "unit"},
                                                            []);
                                                          ptyp_loc_stack = []}]);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_construct
                                                      ({txt = Lident "::"},
                                                      Some
                                                       {pexp_desc =
                                                         Pexp_tuple
                                                          [{pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident "x"};
                                                            pexp_loc_stack =
                                                             []};
                                                           {pexp_desc =
                                                             Pexp_construct
                                                              ({txt =
                                                                 Lident "[]"},
                                                              None);
                                                            pexp_loc_stack =
                                                             []}];
                                                        pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt = Lident "list"},
                                                     [{ptyp_desc =
                                                        Ptyp_constr
                                                         ({txt =
                                                            Lident "unit"},
                                                         []);
                                                       ptyp_loc_stack = []}]);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "xs"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos_xs_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos_xs_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc = Ppat_var {txt = "x"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos_x_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos_x_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "xs"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "list"},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "unit"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "xs"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "list"},
                                         [{ptyp_desc =
                                            Ptyp_constr
                                             ({txt = Lident "unit"}, 
                                             []);
                                           ptyp_loc_stack = []}]);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "x"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "unit"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "x"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos_x_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos_xs_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Lident "list"},
                                                           [{ptyp_desc =
                                                              Ptyp_constr
                                                               ({txt =
                                                                  Lident
                                                                   "unit"},
                                                               []);
                                                             ptyp_loc_stack =
                                                              []}]);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_construct
                                                         ({txt = Lident "::"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_tuple
                                                             [{pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident "x"};
                                                               pexp_loc_stack
                                                                = []};
                                                              {pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "xs"};
                                                               pexp_loc_stack
                                                                = []}];
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt = Lident "list"},
                                                        [{ptyp_desc =
                                                           Ptyp_constr
                                                            ({txt =
                                                               Lident "unit"},
                                                            []);
                                                          ptyp_loc_stack = []}]);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_2"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_2"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_2"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "list"},
                                               [{ptyp_desc =
                                                  Ptyp_constr
                                                   ({txt = Lident "unit"},
                                                   []);
                                                 ptyp_loc_stack = []}]);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "list"},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "unit"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos__2_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Lident "unit"},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_apply
                                                         ({pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident
                                                                "print_endline"};
                                                           pexp_loc_stack =
                                                            []},
                                                         [(Nolabel,
                                                           {pexp_desc =
                                                             Pexp_apply
                                                              ({pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                pexp_loc_stack
                                                                 = []},
                                                              [(Nolabel,
                                                                {pexp_desc =
                                                                  Pexp_constant
                                                                   (Pconst_string
                                                                    ("DEBUG:DONE",
                                                                    None));
                                                                 pexp_loc_stack
                                                                  = []})]);
                                                            pexp_loc_stack =
                                                             []})]);
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt = Lident "unit"},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_sequence
                                                      ({pexp_desc =
                                                         Pexp_apply
                                                          ({pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "print_endline"};
                                                            pexp_loc_stack =
                                                             []},
                                                          [(Nolabel,
                                                            {pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:rhs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []})]);
                                                        pexp_loc_stack =
                                                         []},
                                                      {pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident "NFactor"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_1"};
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc =
                                        Ppat_var {txt = "branches"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var
                                         {txt = "_startpos_branches_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos_branches_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc = Ppat_any;
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_2"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__2_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__2_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_menhir_s"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {ppat_desc =
                                                    Ppat_var {txt = "symbol"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt =
                                                       "_startpos_symbol_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos_symbol_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_menhir_stack"};
                                                   ppat_loc_stack = []})],
                                               Closed);
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc =
                                         Ppat_var {txt = "branches"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "branches"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_2"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "unit"}, 
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_2"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_constraint
                                            ({ppat_desc =
                                               Ppat_var {txt = "symbol"};
                                              ppat_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_poly ([],
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt =
                                                     Ldot
                                                      (Lident "Positions",
                                                      "located")},
                                                  [{ptyp_desc =
                                                     Ptyp_constr
                                                      ({txt = Lident "string"},
                                                      []);
                                                    ptyp_loc_stack = []}]);
                                                ptyp_loc_stack = []});
                                             ptyp_loc_stack = []});
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_constraint
                                            ({pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt =
                                                      Ldot (Lident "Obj",
                                                       "magic")};
                                                  pexp_loc_stack = []},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "symbol"};
                                                   pexp_loc_stack = []})]);
                                              pexp_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Positions",
                                                   "located")},
                                               [{ptyp_desc =
                                                  Ptyp_constr
                                                   ({txt = Lident "string"},
                                                   []);
                                                 ptyp_loc_stack = []}]);
                                             ptyp_loc_stack = []});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__0_"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_field
                                               ({pexp_desc =
                                                  Pexp_ident
                                                   {txt =
                                                     Lident "_menhir_stack"};
                                                 pexp_loc_stack = []},
                                               {txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "endp")});
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_startpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident
                                                     "_startpos_symbol_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident
                                                        "_endpos_branches_"};
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_constraint
                                                        ({ppat_desc =
                                                           Ppat_var
                                                            {txt = "_v"};
                                                          ppat_loc_stack = []},
                                                        {ptyp_desc =
                                                          Ptyp_poly (
                                                           [],
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "parameterized_rule")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         ptyp_loc_stack = []});
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_constraint
                                                        ({pexp_desc =
                                                           Pexp_sequence
                                                            ({pexp_desc =
                                                               Pexp_apply
                                                                ({pexp_desc =
                                                                   Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []},
                                                                [(Nolabel,
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:rule",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "symbol"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "branches"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                              pexp_loc_stack
                                                               = []},
                                                            {pexp_desc =
                                                              Pexp_record
                                                               ([({txt =
                                                                    Lident
                                                                    "pr_nt"},
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Positions",
                                                                    "value")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "symbol"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                 ({txt =
                                                                    Lident
                                                                    "pr_positions"},
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "::"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Positions",
                                                                    "position")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "symbol"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "[]"},
                                                                    None);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                 ({txt =
                                                                    Lident
                                                                    "pr_branches"},
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "[]"},
                                                                    None);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []})],
                                                               None);
                                                             pexp_loc_stack =
                                                              []});
                                                          pexp_loc_stack =
                                                           []},
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "parameterized_rule")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_record
                                                     ([({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "state")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_menhir_s"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "semv")},
                                                        {pexp_desc =
                                                          Pexp_apply
                                                           ({pexp_desc =
                                                              Pexp_ident
                                                               {txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Obj",
                                                                  "repr")};
                                                             pexp_loc_stack =
                                                              []},
                                                           [(Nolabel,
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_v"};
                                                              pexp_loc_stack
                                                               = []})]);
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "startp")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_startpos"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "endp")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident "_endpos"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "next")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_menhir_stack"};
                                                         pexp_loc_stack = []})],
                                                     None);
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_3"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__3_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__3_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc = Ppat_any;
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_2"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__2_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__2_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_menhir_s"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_1"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_startpos__1_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos__1_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_menhir_stack"};
                                                   ppat_loc_stack = []})],
                                               Closed);
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_3"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "parameterized_rule")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_3"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "parameterized_rule")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_2"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "list"},
                                               [{ptyp_desc =
                                                  Ptyp_constr
                                                   ({txt = Lident "unit"},
                                                   []);
                                                 ptyp_loc_stack = []}]);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_2"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "list"},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "unit"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_constraint
                                            ({ppat_desc =
                                               Ppat_var {txt = "_1"};
                                              ppat_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_poly ([],
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt =
                                                     Ldot (Lident "Syntax",
                                                      "myfactor")},
                                                  []);
                                                ptyp_loc_stack = []});
                                             ptyp_loc_stack = []});
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_constraint
                                            ({pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt =
                                                      Ldot (Lident "Obj",
                                                       "magic")};
                                                  pexp_loc_stack = []},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_1"};
                                                   pexp_loc_stack = []})]);
                                              pexp_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__0_"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_field
                                               ({pexp_desc =
                                                  Pexp_ident
                                                   {txt =
                                                     Lident "_menhir_stack"};
                                                 pexp_loc_stack = []},
                                               {txt =
                                                 Ldot
                                                  (Ldot (Lident "MenhirLib",
                                                    "EngineTypes"),
                                                  "endp")});
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_startpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Lident "_startpos__1_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_endpos__3_"};
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_constraint
                                                        ({ppat_desc =
                                                           Ppat_var
                                                            {txt = "_v"};
                                                          ppat_loc_stack = []},
                                                        {ptyp_desc =
                                                          Ptyp_poly (
                                                           [],
                                                           {ptyp_desc =
                                                             Ptyp_constr
                                                              ({txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Syntax",
                                                                  "myfactor")},
                                                              []);
                                                            ptyp_loc_stack =
                                                             []});
                                                         ptyp_loc_stack = []});
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_constraint
                                                        ({pexp_desc =
                                                           Pexp_sequence
                                                            ({pexp_desc =
                                                               Pexp_apply
                                                                ({pexp_desc =
                                                                   Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []},
                                                                [(Nolabel,
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:OLDRULE1",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_3"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                              pexp_loc_stack
                                                               = []},
                                                            {pexp_desc =
                                                              Pexp_construct
                                                               ({txt =
                                                                  Lident
                                                                   "Rule"},
                                                               Some
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_3"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                             pexp_loc_stack =
                                                              []});
                                                          pexp_loc_stack =
                                                           []},
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "myfactor")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_record
                                                     ([({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "state")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_menhir_s"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "semv")},
                                                        {pexp_desc =
                                                          Pexp_apply
                                                           ({pexp_desc =
                                                              Pexp_ident
                                                               {txt =
                                                                 Ldot
                                                                  (Lident
                                                                    "Obj",
                                                                  "repr")};
                                                             pexp_loc_stack =
                                                              []},
                                                           [(Nolabel,
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_v"};
                                                              pexp_loc_stack
                                                               = []})]);
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "startp")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_startpos"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "endp")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident "_endpos"};
                                                         pexp_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "next")},
                                                        {pexp_desc =
                                                          Pexp_ident
                                                           {txt =
                                                             Lident
                                                              "_menhir_stack"};
                                                         pexp_loc_stack = []})],
                                                     None);
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_2"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__2_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_menhir_s"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_1"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__1_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_menhir_stack"};
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_2"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "parameterized_rule")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_2"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "parameterized_rule")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_1"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "list"},
                                               [{ptyp_desc =
                                                  Ptyp_constr
                                                   ({txt = Lident "unit"},
                                                   []);
                                                 ptyp_loc_stack = []}]);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_1"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "list"},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "unit"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_endpos__0_"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_field
                                            ({pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "_menhir_stack"};
                                              pexp_loc_stack = []},
                                            {txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_startpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_startpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_endpos"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_endpos__2_"};
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_constraint
                                                     ({ppat_desc =
                                                        Ppat_var {txt = "_v"};
                                                       ppat_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_poly ([],
                                                        {ptyp_desc =
                                                          Ptyp_constr
                                                           ({txt =
                                                              Ldot
                                                               (Lident
                                                                 "Syntax",
                                                               "myfactor")},
                                                           []);
                                                         ptyp_loc_stack = []});
                                                      ptyp_loc_stack = []});
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:OLDRULE",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident "Rule"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_2"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_record
                                                  ([({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "state")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_menhir_s"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "semv")},
                                                     {pexp_desc =
                                                       Pexp_apply
                                                        ({pexp_desc =
                                                           Pexp_ident
                                                            {txt =
                                                              Ldot
                                                               (Lident "Obj",
                                                               "repr")};
                                                          pexp_loc_stack = []},
                                                        [(Nolabel,
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_v"};
                                                           pexp_loc_stack =
                                                            []})]);
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "startp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_startpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "endp")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident "_endpos"};
                                                      pexp_loc_stack = []});
                                                    ({txt =
                                                       Ldot
                                                        (Ldot
                                                          (Lident "MenhirLib",
                                                          "EngineTypes"),
                                                        "next")},
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_menhir_stack"};
                                                      pexp_loc_stack = []})],
                                                  None);
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "parameterized_rule")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "parameterized_rule")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_constraint
                                                  ({ppat_desc =
                                                     Ppat_var {txt = "_v"};
                                                    ppat_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_poly ([],
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   ptyp_loc_stack = []});
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_constraint
                                                  ({pexp_desc =
                                                     Pexp_sequence
                                                      ({pexp_desc =
                                                         Pexp_apply
                                                          ({pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "print_endline"};
                                                            pexp_loc_stack =
                                                             []},
                                                          [(Nolabel,
                                                            {pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:OLDRULE",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []})]);
                                                        pexp_loc_stack =
                                                         []},
                                                      {pexp_desc =
                                                        Pexp_construct
                                                         ({txt =
                                                            Lident "Rule"},
                                                         Some
                                                          {pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Lident "_1"};
                                                           pexp_loc_stack =
                                                            []});
                                                       pexp_loc_stack = []});
                                                    pexp_loc_stack = []},
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt =
                                                        Ldot
                                                         (Lident "Syntax",
                                                         "myfactor")},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc = Ppat_any;
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_4"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__4_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__4_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_record
                                         ([({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "state")},
                                            {ppat_desc = Ppat_any;
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "semv")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_3"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "startp")},
                                            {ppat_desc =
                                              Ppat_var
                                               {txt = "_startpos__3_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "endp")},
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos__3_"};
                                             ppat_loc_stack = []});
                                           ({txt =
                                              Ldot
                                               (Ldot (Lident "MenhirLib",
                                                 "EngineTypes"),
                                               "next")},
                                            {ppat_desc =
                                              Ppat_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {ppat_desc = Ppat_any;
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {ppat_desc =
                                                    Ppat_var {txt = "_2"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_startpos__2_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_endpos__2_"};
                                                   ppat_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {ppat_desc =
                                                    Ppat_record
                                                     ([({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "state")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_menhir_s"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "semv")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_1"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "startp")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt =
                                                             "_startpos__1_"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "endp")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt =
                                                             "_endpos__1_"};
                                                         ppat_loc_stack = []});
                                                       ({txt =
                                                          Ldot
                                                           (Ldot
                                                             (Lident
                                                               "MenhirLib",
                                                             "EngineTypes"),
                                                           "next")},
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt =
                                                             "_menhir_stack"};
                                                         ppat_loc_stack = []})],
                                                     Closed);
                                                   ppat_loc_stack = []})],
                                               Closed);
                                             ppat_loc_stack = []})],
                                         Closed);
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_4"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt = Lident "unit"}, []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_4"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr ({txt = Lident "unit"},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_constraint
                                         ({ppat_desc = Ppat_var {txt = "_3"};
                                           ppat_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_poly ([],
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt =
                                                  Ldot (Lident "Syntax",
                                                   "myfactor")},
                                               []);
                                             ptyp_loc_stack = []});
                                          ptyp_loc_stack = []});
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_constraint
                                         ({pexp_desc =
                                            Pexp_apply
                                             ({pexp_desc =
                                                Pexp_ident
                                                 {txt =
                                                   Ldot (Lident "Obj",
                                                    "magic")};
                                               pexp_loc_stack = []},
                                             [(Nolabel,
                                               {pexp_desc =
                                                 Pexp_ident
                                                  {txt = Lident "_3"};
                                                pexp_loc_stack = []})]);
                                           pexp_loc_stack = []},
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_constraint
                                            ({ppat_desc =
                                               Ppat_var {txt = "_2"};
                                              ppat_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_poly ([],
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt = Lident "list"},
                                                  [{ptyp_desc =
                                                     Ptyp_constr
                                                      ({txt = Lident "unit"},
                                                      []);
                                                    ptyp_loc_stack = []}]);
                                                ptyp_loc_stack = []});
                                             ptyp_loc_stack = []});
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_constraint
                                            ({pexp_desc =
                                               Pexp_apply
                                                ({pexp_desc =
                                                   Pexp_ident
                                                    {txt =
                                                      Ldot (Lident "Obj",
                                                       "magic")};
                                                  pexp_loc_stack = []},
                                                [(Nolabel,
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_2"};
                                                   pexp_loc_stack = []})]);
                                              pexp_loc_stack = []},
                                            {ptyp_desc =
                                              Ptyp_constr
                                               ({txt = Lident "list"},
                                               [{ptyp_desc =
                                                  Ptyp_constr
                                                   ({txt = Lident "unit"},
                                                   []);
                                                 ptyp_loc_stack = []}]);
                                             ptyp_loc_stack = []});
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_constraint
                                               ({ppat_desc =
                                                  Ppat_var {txt = "_1"};
                                                 ppat_loc_stack = []},
                                               {ptyp_desc =
                                                 Ptyp_poly ([],
                                                  {ptyp_desc =
                                                    Ptyp_constr
                                                     ({txt = Lident "unit"},
                                                     []);
                                                   ptyp_loc_stack = []});
                                                ptyp_loc_stack = []});
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_constraint
                                               ({pexp_desc =
                                                  Pexp_apply
                                                   ({pexp_desc =
                                                      Pexp_ident
                                                       {txt =
                                                         Ldot (Lident "Obj",
                                                          "magic")};
                                                     pexp_loc_stack = []},
                                                   [(Nolabel,
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt = Lident "_1"};
                                                      pexp_loc_stack = []})]);
                                                 pexp_loc_stack = []},
                                               {ptyp_desc =
                                                 Ptyp_constr
                                                  ({txt = Lident "unit"}, 
                                                  []);
                                                ptyp_loc_stack = []});
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var
                                                  {txt = "_endpos__0_"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_field
                                                  ({pexp_desc =
                                                     Pexp_ident
                                                      {txt =
                                                        Lident
                                                         "_menhir_stack"};
                                                    pexp_loc_stack = []},
                                                  {txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_let (Nonrecursive,
                                               [{pvb_pat =
                                                  {ppat_desc =
                                                    Ppat_var
                                                     {txt = "_startpos"};
                                                   ppat_loc_stack = []};
                                                 pvb_expr =
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos__1_"};
                                                   pexp_loc_stack = []}}],
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var
                                                        {txt = "_endpos"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_ident
                                                        {txt =
                                                          Lident
                                                           "_endpos__4_"};
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_let (Nonrecursive,
                                                     [{pvb_pat =
                                                        {ppat_desc =
                                                          Ppat_var
                                                           {txt = "_v"};
                                                         ppat_loc_stack = []};
                                                       pvb_expr =
                                                        {pexp_desc =
                                                          Pexp_let
                                                           (Nonrecursive,
                                                           [{pvb_pat =
                                                              {ppat_desc =
                                                                Ppat_var
                                                                 {txt = "_1"};
                                                               ppat_loc_stack
                                                                = []};
                                                             pvb_expr =
                                                              {pexp_desc =
                                                                Pexp_let
                                                                 (Nonrecursive,
                                                                 [{pvb_pat =
                                                                    {ppat_desc
                                                                    =
                                                                    Ppat_var
                                                                    {txt =
                                                                    "_1"};
                                                                    ppat_loc_stack
                                                                    = 
                                                                    []};
                                                                   pvb_expr =
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_sequence
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:rhs",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_3"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_3"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}}],
                                                                 {pexp_desc =
                                                                   Pexp_sequence
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cterm/group",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []});
                                                               pexp_loc_stack
                                                                = []}}],
                                                           {pexp_desc =
                                                             Pexp_constraint
                                                              ({pexp_desc =
                                                                 Pexp_sequence
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:term/cterms",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                pexp_loc_stack
                                                                 = []},
                                                              {ptyp_desc =
                                                                Ptyp_constr
                                                                 ({txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Syntax",
                                                                    "myfactor")},
                                                                 []);
                                                               ptyp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack = []}}],
                                                     {pexp_desc =
                                                       Pexp_record
                                                        ([({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "state")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_menhir_s"};
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "semv")},
                                                           {pexp_desc =
                                                             Pexp_apply
                                                              ({pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Obj",
                                                                    "repr")};
                                                                pexp_loc_stack
                                                                 = []},
                                                              [(Nolabel,
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "_v"};
                                                                 pexp_loc_stack
                                                                  = []})]);
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "startp")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_startpos"};
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "endp")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_endpos"};
                                                            pexp_loc_stack =
                                                             []});
                                                          ({txt =
                                                             Ldot
                                                              (Ldot
                                                                (Lident
                                                                  "MenhirLib",
                                                                "EngineTypes"),
                                                              "next")},
                                                           {pexp_desc =
                                                             Pexp_ident
                                                              {txt =
                                                                Lident
                                                                 "_menhir_stack"};
                                                            pexp_loc_stack =
                                                             []})],
                                                        None);
                                                      pexp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []});
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Syntax",
                                                "myfactor")},
                                            []);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Syntax",
                                             "myfactor")},
                                         []);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_v"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var {txt = "_1"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:class1a",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "NFactor"},
                                                                 Some
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_sequence
                                                           ({pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_apply
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cterm/class",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []},
                                                           {pexp_desc =
                                                             Pexp_construct
                                                              ({txt =
                                                                 Lident
                                                                  "NFactor"},
                                                              Some
                                                               {pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Lident
                                                                    "_1"};
                                                                pexp_loc_stack
                                                                 = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack =
                                                          []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:term/cterms",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_1"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Positions",
                                                "located")},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "string"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Positions",
                                             "located")},
                                         [{ptyp_desc =
                                            Ptyp_constr
                                             ({txt = Lident "string"}, 
                                             []);
                                           ptyp_loc_stack = []}]);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_v"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var {txt = "_1"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:class",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_construct
                                                                 ({txt =
                                                                    Lident
                                                                    "SFactor"},
                                                                 Some
                                                                  {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                   pexp_loc_stack
                                                                    = 
                                                                    []});
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_sequence
                                                           ({pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_apply
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:cterm/class",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []},
                                                           {pexp_desc =
                                                             Pexp_construct
                                                              ({txt =
                                                                 Lident
                                                                  "NFactor"},
                                                              Some
                                                               {pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Lident
                                                                    "_1"};
                                                                pexp_loc_stack
                                                                 = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack =
                                                          []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:term/cterms",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_1"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Positions",
                                                "located")},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "string"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Positions",
                                             "located")},
                                         [{ptyp_desc =
                                            Ptyp_constr
                                             ({txt = Lident "string"}, 
                                             []);
                                           ptyp_loc_stack = []}]);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_v"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var {txt = "_1"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:quid",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "_1"};
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_sequence
                                                           ({pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_apply
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:sterm/quid",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []},
                                                           {pexp_desc =
                                                             Pexp_construct
                                                              ({txt =
                                                                 Lident
                                                                  "SFactor"},
                                                              Some
                                                               {pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Lident
                                                                    "_1"};
                                                                pexp_loc_stack
                                                                 = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack =
                                                          []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:term/sterm",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_1"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []};
                    {pexp_desc =
                      Pexp_fun (Nolabel, None,
                       {ppat_desc = Ppat_var {txt = "_menhir_env"};
                        ppat_loc_stack = []},
                       {pexp_desc =
                         Pexp_let (Nonrecursive,
                          [{pvb_pat =
                             {ppat_desc = Ppat_var {txt = "_menhir_stack"};
                              ppat_loc_stack = []};
                            pvb_expr =
                             {pexp_desc =
                               Pexp_field
                                ({pexp_desc =
                                   Pexp_ident {txt = Lident "_menhir_env"};
                                  pexp_loc_stack = []},
                                {txt =
                                  Ldot
                                   (Ldot (Lident "MenhirLib", "EngineTypes"),
                                   "stack")});
                              pexp_loc_stack = []}}],
                          {pexp_desc =
                            Pexp_let (Nonrecursive,
                             [{pvb_pat =
                                {ppat_desc =
                                  Ppat_record
                                   ([({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "state")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_s"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "semv")},
                                      {ppat_desc = Ppat_var {txt = "_1"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "startp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_startpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "endp")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__1_"};
                                       ppat_loc_stack = []});
                                     ({txt =
                                        Ldot
                                         (Ldot (Lident "MenhirLib",
                                           "EngineTypes"),
                                         "next")},
                                      {ppat_desc =
                                        Ppat_var {txt = "_menhir_stack"};
                                       ppat_loc_stack = []})],
                                   Closed);
                                 ppat_loc_stack = []};
                               pvb_expr =
                                {pexp_desc =
                                  Pexp_ident {txt = Lident "_menhir_stack"};
                                 pexp_loc_stack = []}}],
                             {pexp_desc =
                               Pexp_let (Nonrecursive,
                                [{pvb_pat =
                                   {ppat_desc =
                                     Ppat_constraint
                                      ({ppat_desc = Ppat_var {txt = "_1"};
                                        ppat_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_poly ([],
                                         {ptyp_desc =
                                           Ptyp_constr
                                            ({txt =
                                               Ldot (Lident "Positions",
                                                "located")},
                                            [{ptyp_desc =
                                               Ptyp_constr
                                                ({txt = Lident "string"}, 
                                                []);
                                              ptyp_loc_stack = []}]);
                                          ptyp_loc_stack = []});
                                       ptyp_loc_stack = []});
                                    ppat_loc_stack = []};
                                  pvb_expr =
                                   {pexp_desc =
                                     Pexp_constraint
                                      ({pexp_desc =
                                         Pexp_apply
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt =
                                                Ldot (Lident "Obj", "magic")};
                                            pexp_loc_stack = []},
                                          [(Nolabel,
                                            {pexp_desc =
                                              Pexp_ident {txt = Lident "_1"};
                                             pexp_loc_stack = []})]);
                                        pexp_loc_stack = []},
                                      {ptyp_desc =
                                        Ptyp_constr
                                         ({txt =
                                            Ldot (Lident "Positions",
                                             "located")},
                                         [{ptyp_desc =
                                            Ptyp_constr
                                             ({txt = Lident "string"}, 
                                             []);
                                           ptyp_loc_stack = []}]);
                                       ptyp_loc_stack = []});
                                    pexp_loc_stack = []}}],
                                {pexp_desc =
                                  Pexp_let (Nonrecursive,
                                   [{pvb_pat =
                                      {ppat_desc =
                                        Ppat_var {txt = "_endpos__0_"};
                                       ppat_loc_stack = []};
                                     pvb_expr =
                                      {pexp_desc =
                                        Pexp_field
                                         ({pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "_menhir_stack"};
                                           pexp_loc_stack = []},
                                         {txt =
                                           Ldot
                                            (Ldot (Lident "MenhirLib",
                                              "EngineTypes"),
                                            "endp")});
                                       pexp_loc_stack = []}}],
                                   {pexp_desc =
                                     Pexp_let (Nonrecursive,
                                      [{pvb_pat =
                                         {ppat_desc =
                                           Ppat_var {txt = "_startpos"};
                                          ppat_loc_stack = []};
                                        pvb_expr =
                                         {pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "_startpos__1_"};
                                          pexp_loc_stack = []}}],
                                      {pexp_desc =
                                        Pexp_let (Nonrecursive,
                                         [{pvb_pat =
                                            {ppat_desc =
                                              Ppat_var {txt = "_endpos"};
                                             ppat_loc_stack = []};
                                           pvb_expr =
                                            {pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "_endpos__1_"};
                                             pexp_loc_stack = []}}],
                                         {pexp_desc =
                                           Pexp_let (Nonrecursive,
                                            [{pvb_pat =
                                               {ppat_desc =
                                                 Ppat_var {txt = "_v"};
                                                ppat_loc_stack = []};
                                              pvb_expr =
                                               {pexp_desc =
                                                 Pexp_let (Nonrecursive,
                                                  [{pvb_pat =
                                                     {ppat_desc =
                                                       Ppat_var {txt = "_1"};
                                                      ppat_loc_stack = []};
                                                    pvb_expr =
                                                     {pexp_desc =
                                                       Pexp_let
                                                        (Nonrecursive,
                                                        [{pvb_pat =
                                                           {ppat_desc =
                                                             Ppat_var
                                                              {txt = "_1"};
                                                            ppat_loc_stack =
                                                             []};
                                                          pvb_expr =
                                                           {pexp_desc =
                                                             Pexp_sequence
                                                              ({pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:lid",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []},
                                                              {pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "_1"};
                                                               pexp_loc_stack
                                                                = []});
                                                            pexp_loc_stack =
                                                             []}}],
                                                        {pexp_desc =
                                                          Pexp_sequence
                                                           ({pexp_desc =
                                                              Pexp_apply
                                                               ({pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "print_endline"};
                                                                 pexp_loc_stack
                                                                  = []},
                                                               [(Nolabel,
                                                                 {pexp_desc =
                                                                   Pexp_apply
                                                                    (
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:sterm/lid",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                  pexp_loc_stack
                                                                   = 
                                                                   []})]);
                                                             pexp_loc_stack =
                                                              []},
                                                           {pexp_desc =
                                                             Pexp_construct
                                                              ({txt =
                                                                 Lident
                                                                  "SFactor"},
                                                              Some
                                                               {pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Lident
                                                                    "_1"};
                                                                pexp_loc_stack
                                                                 = []});
                                                            pexp_loc_stack =
                                                             []});
                                                         pexp_loc_stack =
                                                          []});
                                                      pexp_loc_stack = []}}],
                                                  {pexp_desc =
                                                    Pexp_constraint
                                                     ({pexp_desc =
                                                        Pexp_sequence
                                                         ({pexp_desc =
                                                            Pexp_apply
                                                             ({pexp_desc =
                                                                Pexp_ident
                                                                 {txt =
                                                                   Lident
                                                                    "print_endline"};
                                                               pexp_loc_stack
                                                                = []},
                                                             [(Nolabel,
                                                               {pexp_desc =
                                                                 Pexp_apply
                                                                  ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Batteries",
                                                                    "dump")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                  [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_constant
                                                                    (Pconst_string
                                                                    ("DEBUG:term/sterm",
                                                                    None));
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "_1"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                pexp_loc_stack
                                                                 = []})]);
                                                           pexp_loc_stack =
                                                            []},
                                                         {pexp_desc =
                                                           Pexp_construct
                                                            ({txt =
                                                               Lident
                                                                "NFactor"},
                                                            Some
                                                             {pexp_desc =
                                                               Pexp_ident
                                                                {txt =
                                                                  Lident "_1"};
                                                              pexp_loc_stack
                                                               = []});
                                                          pexp_loc_stack = []});
                                                       pexp_loc_stack = []},
                                                     {ptyp_desc =
                                                       Ptyp_constr
                                                        ({txt =
                                                           Ldot
                                                            (Lident "Syntax",
                                                            "myfactor")},
                                                        []);
                                                      ptyp_loc_stack = []});
                                                   pexp_loc_stack = []});
                                                pexp_loc_stack = []}}],
                                            {pexp_desc =
                                              Pexp_record
                                               ([({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "state")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_s"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "semv")},
                                                  {pexp_desc =
                                                    Pexp_apply
                                                     ({pexp_desc =
                                                        Pexp_ident
                                                         {txt =
                                                           Ldot
                                                            (Lident "Obj",
                                                            "repr")};
                                                       pexp_loc_stack = []},
                                                     [(Nolabel,
                                                       {pexp_desc =
                                                         Pexp_ident
                                                          {txt = Lident "_v"};
                                                        pexp_loc_stack = []})]);
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "startp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_startpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "endp")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt = Lident "_endpos"};
                                                   pexp_loc_stack = []});
                                                 ({txt =
                                                    Ldot
                                                     (Ldot
                                                       (Lident "MenhirLib",
                                                       "EngineTypes"),
                                                     "next")},
                                                  {pexp_desc =
                                                    Pexp_ident
                                                     {txt =
                                                       Lident "_menhir_stack"};
                                                   pexp_loc_stack = []})],
                                               None);
                                             pexp_loc_stack = []});
                                          pexp_loc_stack = []});
                                       pexp_loc_stack = []});
                                    pexp_loc_stack = []});
                                 pexp_loc_stack = []});
                              pexp_loc_stack = []});
                           pexp_loc_stack = []});
                        pexp_loc_stack = []});
                     pexp_loc_stack = []}];
                 pexp_loc_stack = []}};
              {pvb_pat =
                {ppat_desc = Ppat_var {txt = "trace"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_construct ({txt = Lident "Some"},
                   Some
                    {pexp_desc =
                      Pexp_tuple
                       [{pexp_desc =
                          Pexp_array
                           [{pexp_desc =
                              Pexp_constant (Pconst_string ("error", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("Tchar", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("STAR", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("RPAREN", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("REGEX", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string ("QUESTION", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("QID", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("PLUS", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("NEWLINE", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("LPAREN", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("LID", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("EOF", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("DASH", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string ("COLONCOLONEQUAL", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("CARET", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("BAR", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant (Pconst_string ("#", None));
                             pexp_loc_stack = []}];
                         pexp_loc_stack = []};
                        {pexp_desc =
                          Pexp_array
                           [{pexp_desc =
                              Pexp_constant
                               (Pconst_string ("Accepting", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production alternation -> alternation BAR list(NEWLINE) concatenation",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production alternation -> concatenation",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production char_class -> CARET char_class1",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production char_class -> char_class1",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production char_class1 -> Tchar DASH Tchar",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production char_class1 -> char_class1 Tchar",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production char_class1 -> Tchar",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production concatenation -> concatenation factor",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production concatenation -> factor",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production factor -> term PLUS",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production factor -> term QUESTION",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production factor -> term STAR",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production factor -> term", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production grammar -> rules postlude",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production list(NEWLINE) ->",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production list(NEWLINE) -> NEWLINE list(NEWLINE)",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production nonempty_list(NEWLINE) -> NEWLINE",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production nonempty_list(NEWLINE) -> NEWLINE nonempty_list(NEWLINE)",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production postlude -> list(NEWLINE) EOF",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production rhs -> alternation",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production rule -> LID COLONCOLONEQUAL rhs",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production rules -> rules nonempty_list(NEWLINE) rule",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production rules -> nonempty_list(NEWLINE) rule",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production rules -> rule", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production term -> LPAREN list(NEWLINE) rhs RPAREN",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production term -> char_class",
                                 None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production term -> REGEX", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production term -> QID", None));
                             pexp_loc_stack = []};
                            {pexp_desc =
                              Pexp_constant
                               (Pconst_string
                                 ("Reducing production term -> LID", None));
                             pexp_loc_stack = []}];
                         pexp_loc_stack = []}];
                     pexp_loc_stack = []});
                 pexp_loc_stack = []}}])}]}}};
 {pstr_desc =
   Pstr_module
    {pmb_name = {txt = Some "MenhirInterpreter"};
     pmb_expr =
      {pmod_desc =
        Pmod_structure
         [{pstr_desc =
            Pstr_module
             {pmb_name = {txt = Some "ET"};
              pmb_expr =
               {pmod_desc =
                 Pmod_apply
                  ({pmod_desc =
                     Pmod_ident
                      {txt =
                        Ldot (Ldot (Lident "MenhirLib", "TableInterpreter"),
                         "MakeEngineTable")}},
                  {pmod_desc = Pmod_ident {txt = Lident "Tables"}})}}};
          {pstr_desc =
            Pstr_module
             {pmb_name = {txt = Some "TI"};
              pmb_expr =
               {pmod_desc =
                 Pmod_apply
                  ({pmod_desc =
                     Pmod_ident
                      {txt =
                        Ldot (Ldot (Lident "MenhirLib", "Engine"), "Make")}},
                  {pmod_desc = Pmod_ident {txt = Lident "ET"}})}}};
          {pstr_desc =
            Pstr_include
             {pincl_mod = {pmod_desc = Pmod_ident {txt = Lident "TI"}}}}]}}};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "grammar"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "lexer"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Nolabel, None,
             {ppat_desc = Ppat_var {txt = "lexbuf"}; ppat_loc_stack = []},
             {pexp_desc =
               Pexp_constraint
                ({pexp_desc =
                   Pexp_apply
                    ({pexp_desc =
                       Pexp_ident {txt = Ldot (Lident "Obj", "magic")};
                      pexp_loc_stack = []},
                    [(Nolabel,
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident
                             {txt =
                               Ldot (Lident "MenhirInterpreter", "entry")};
                           pexp_loc_stack = []},
                         [(Nolabel,
                           {pexp_desc = Pexp_variant ("Legacy", None);
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc =
                             Pexp_constant (Pconst_integer ("0", None));
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "lexer"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "lexbuf"};
                            pexp_loc_stack = []})]);
                       pexp_loc_stack = []})]);
                  pexp_loc_stack = []},
                {ptyp_desc =
                  Ptyp_constr
                   ({txt = Ldot (Lident "Syntax", "partial_grammar")}, 
                   []);
                 ptyp_loc_stack = []});
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_module
    {pmb_name = {txt = Some "Incremental"};
     pmb_expr =
      {pmod_desc =
        Pmod_structure
         [{pstr_desc =
            Pstr_value (Nonrecursive,
             [{pvb_pat =
                {ppat_desc = Ppat_var {txt = "grammar"}; ppat_loc_stack = []};
               pvb_expr =
                {pexp_desc =
                  Pexp_fun (Nolabel, None,
                   {ppat_desc = Ppat_var {txt = "initial_position"};
                    ppat_loc_stack = []},
                   {pexp_desc =
                     Pexp_constraint
                      ({pexp_desc =
                         Pexp_apply
                          ({pexp_desc =
                             Pexp_ident {txt = Ldot (Lident "Obj", "magic")};
                            pexp_loc_stack = []},
                          [(Nolabel,
                            {pexp_desc =
                              Pexp_apply
                               ({pexp_desc =
                                  Pexp_ident
                                   {txt =
                                     Ldot (Lident "MenhirInterpreter",
                                      "start")};
                                 pexp_loc_stack = []},
                               [(Nolabel,
                                 {pexp_desc =
                                   Pexp_constant (Pconst_integer ("0", None));
                                  pexp_loc_stack = []});
                                (Nolabel,
                                 {pexp_desc =
                                   Pexp_ident
                                    {txt = Lident "initial_position"};
                                  pexp_loc_stack = []})]);
                             pexp_loc_stack = []})]);
                        pexp_loc_stack = []},
                      {ptyp_desc =
                        Ptyp_constr
                         ({txt =
                            Ldot (Lident "MenhirInterpreter", "checkpoint")},
                         [{ptyp_desc =
                            Ptyp_constr
                             ({txt =
                                Ldot (Lident "Syntax", "partial_grammar")},
                             []);
                           ptyp_loc_stack = []}]);
                       ptyp_loc_stack = []});
                    pexp_loc_stack = []});
                 pexp_loc_stack = []}}])}]}}}]


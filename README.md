# Proof Tree Drawer for CFWAE
A proof tree drawer for CFWAE (Conditionals, Functions, "With"'s, Arithmetic Expressions) with automatic reducing for long expressions and values.


## Run Online
https://kjh618.github.io/KAIST-CS320-ProofTree/


## Examples
* Input: `examples/add.txt`, Output:
```
               ∅ ⊢ 2 ⇒ 2  ∅ ⊢ 1 ⇒ 1    
             ――――――――――――――――――――――――  
  ∅ ⊢ 4 ⇒ 4    ∅ ⊢ {- 2 1} ⇒ 1         
―――――――――――――――――――――――――――――――――――――――
  ∅ ⊢ {+ 4 {- 2 1}} ⇒ 5  
```

* Input: `examples/fun.txt`, Output:
```
                                                                                                                                          x ∈ [x]                                 
                                                                                                                                        ―――――――――――――――――――                       
                                               add1 ∈ [add1]                                                                              [x ↦ 2] ⊢ x ⇒ 2    [x ↦ 2] ⊢ 1 ⇒ 1      
                                             ―――――――――――――――――――――――――――――――――――――――――――――――――――――                                    ――――――――――――――――――――――――――――――――――――――――    
                                               [add1 ↦ ⟨λx.{+ x 1}, ∅⟩] ⊢ add1 ⇒ ⟨λx.{+ x 1}, ∅⟩    [add1 ↦ ⟨λx.{+ x 1}, ∅⟩] ⊢ 2 ⇒ 2    [x ↦ 2] ⊢ {+ x 1} ⇒ 3                     
                                           ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――  
  ∅ ⊢ {fun {x} {+ x 1}} ⇒ ⟨λx.{+ x 1}, ∅⟩    [add1 ↦ ⟨λx.{+ x 1}, ∅⟩] ⊢ {add1 2} ⇒ 3                                                                                              
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
  ∅ ⊢ {with {add1 {fun {x} {+ x 1}}} {add1 2}} ⇒ 3  
```

* Input: `examples/fac_1.txt`, Output:
```
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    facY ∈ [facY, x]                    facY ∈ [facY, x]                    [facY ↦ v0] ⊢ {fun {x} {{facY facY} x}} ⇒ v1  [facY ↦ v0, fac ↦ v1] ⊢ {fun {n} e1} ⇒ v2                                          n ∈ [facY, fac, n]                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ――――――――――――――――――――――――――――――――――  ――――――――――――――――――――――――――――――――――  ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――                                      ――――――――――――――――――――――――――――――――――――――――                                                    
                                                                                                                                                                                                                                                                                                                                                                                              n ∈ [facY, fac, n]                                                                    [facY ↦ v0, x ↦ 0] ⊢ facY ⇒ v0      [facY ↦ v0, x ↦ 0] ⊢ facY ⇒ v0      [facY ↦ v0] ⊢ {with {fac e2} e3} ⇒ v2                                                          x ∈ [facY, x]                     [facY ↦ v0, fac ↦ v1, n ↦ 0] ⊢ n ⇒ 0    [facY ↦ v0, fac ↦ v1, n ↦ 0] ⊢ 1 ⇒ 1              
                                                                                                                                                                                                                                                                                                                                                                                            ――――――――――――――――――――――――――――――――――――――――                                            ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――  ――――――――――――――――――――――――――――――  ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――            
                                                                                                                                                                                                                                                                                                                                               fac ∈ [facY, fac, n]                           [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ n ⇒ 1    [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ 1 ⇒ 1        [facY ↦ v0, x ↦ 0] ⊢ {facY facY} ⇒ v2                                                                                                                                    [facY ↦ v0, x ↦ 0] ⊢ x ⇒ 0      [facY ↦ v0, fac ↦ v1, n ↦ 0] ⊢ {if0 n 1 {* n {fac {- n 1}}}} ⇒ 1                            
                                                                                                                                                                                                                                                                                                                                             ―――――――――――――――――――――――――――――――――――――――――――  ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――  ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――          
                                                                                                                                                                                                                                                                                                   n ∈ [facY, fac, n]                          [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ fac ⇒ v1      [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ {- n 1} ⇒ 0                                          [facY ↦ v0, x ↦ 0] ⊢ {{facY facY} x} ⇒ 1                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                 ――――――――――――――――――――――――――――――――――――――――  ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――        
                                  facX ∈ [facX]                facX ∈ [facX]                [facY ↦ v0] ⊢ {fun {x} {{facY facY} x}} ⇒ v1  [facY ↦ v0, fac ↦ v1] ⊢ {fun {n} e1} ⇒ v2                                                             n ∈ [facY, fac, n]                                 [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ n ⇒ 1      [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ {fac {- n 1}} ⇒ 1                                                                                                                                                                                                                                                                                                                                                                                          
                                ―――――――――――――――――――――――――――  ―――――――――――――――――――――――――――  ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――                                                         ――――――――――――――――――――――――――――――――――――――――         ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――      
                                  [facX ↦ v0] ⊢ facX ⇒ v0      [facX ↦ v0] ⊢ facX ⇒ v0      [facY ↦ v0] ⊢ {with {fac e2} e3} ⇒ v2                                                              fac ∈ [fac]                                      [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ n ⇒ 1    1 ≠ 0    [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ {* n {fac {- n 1}}} ⇒ 1                                                                                                                                                                                                                                                                                                                                                                                                                                
                              ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――      ―――――――――――――――――――――――――                      ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――    
    ∅ ⊢ {fun {facY} e0} ⇒ v0    [facX ↦ v0] ⊢ {facX facX} ⇒ v2                                                                                                                                 [fac ↦ v2] ⊢ fac ⇒ v2    [fac ↦ v2] ⊢ 1 ⇒ 1    [facY ↦ v0, fac ↦ v1, n ↦ 1] ⊢ {if0 n 1 {* n {fac {- n 1}}}} ⇒ 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
  ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――  ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――  
    ∅ ⊢ {with {facX e4} {facX facX}} ⇒ v2                                                                                                                                                    [fac ↦ v2] ⊢ {fac 1} ⇒ 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
  ∅ ⊢ {with {fac e5} {fac 1}} ⇒ 1  
* e0 = {with {fac e2} e3}                       = {with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}
* e1 = {if0 n 1 {* n {fac {- n 1}}}}            = {if0 n 1 {* n {fac {- n 1}}}}
* e2 = {fun {x} {{facY facY} x}}                = {fun {x} {{facY facY} x}}
* e3 = {fun {n} e1}                             = {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}
* e4 = {fun {facY} e0}                          = {fun {facY} {with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}}
* e5 = {with {facX e4} {facX facX}}             = {with {facX {fun {facY} {with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}}} {facX facX}}
* v0 = ⟨λfacY.e0, ∅⟩                            = ⟨λfacY.{with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}, ∅⟩
* v1 = ⟨λx.{{facY facY} x}, [facY ↦ v0]⟩        = ⟨λx.{{facY facY} x}, [facY ↦ ⟨λfacY.{with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}, ∅⟩]⟩
* v2 = ⟨λn.e1, [facY ↦ v0, fac ↦ v1]⟩           = ⟨λn.{if0 n 1 {* n {fac {- n 1}}}}, [facY ↦ ⟨λfacY.{with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}, ∅⟩, fac ↦ ⟨λx.{{facY facY} x}, [facY ↦ ⟨λfacY.{with {fac {fun {x} {{facY facY} x}}} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}, ∅⟩]⟩]⟩
```

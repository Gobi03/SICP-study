(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))

 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))

(controller
 test-b                           ; ラベル
   (test =)                       ; テスト
   (branch (label gcd-done))      ; 条件分岐
   (t<-r)                         ; ボタン押す
   (a<-b)                         ; ボタン押す
   (b<-t)                         ; ボタン押す
   (goto (label test-b))          ; 無条件分岐
 gcd-done)                        ; ラベル

(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)


;; 1.4
(controller
   (assign continue (label fact-done))     ; 最終帰り番地設定
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;;nとcontinueを退避し再帰呼出しを設定する.
   ;; 再帰呼出しから戻る時after-fact}から
   ;; 計算が続行するようにcontinueを設定
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; valに n(n-1)!がある
   (goto (reg continue))                   ; 呼出し側に戻る
 base-case
   (assign val (const 1))                  ; 基底の場合: 1!=1
   (goto (reg continue))                   ; 呼出し側に戻る
 fact-done)


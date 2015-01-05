(require 'bts)
(require 'el-expectations)

(expectations
  (desc "complex-condition-validation word")
  (expect nil
    (bts:complex-condition-validation nil "hoge"))
  (desc "complex-condition-validation one operator")
  (expect nil
    (bts:complex-condition-validation nil "hoge|fuga"))
  (desc "complex-condition-validation single list")
  (expect nil
    (bts:complex-condition-validation nil "( hoge & fuga ) | bar"))
  (desc "complex-condition-validation nested list")
  (expect nil
    (bts:complex-condition-validation nil "(hoge & (fuga|bar)) | (foo&baz)"))
  (desc "complex-condition-validation mixed operator")
  (expect "Mixed &,| in a list"
    (let ((current-language-environment "English"))
      (bts:complex-condition-validation nil "hoge|fuga&bar")))
  (desc "complex-condition-validation empty brace")
  (expect "Empty brace"
    (let ((current-language-environment "English"))
      (bts:complex-condition-validation nil "(hoge & ( )) | (foo&baz)")))
  )

(expectations
  (desc "complex-condition-match-to-string word match")
  (expect t
    (bts:complex-condition-match-to-string
     "ahogege"
     (bts:complex-condition-compile "hoge")))
  (desc "complex-condition-match-to-string word unmatch")
  (expect nil
    (bts:complex-condition-match-to-string
     "ahofuga"
     (bts:complex-condition-compile "hoge")))
  (desc "complex-condition-match-to-string operator or match")
  (expect t
    (bts:complex-condition-match-to-string
     "ahogege"
     (bts:complex-condition-compile "hoge|fuga")))
  (desc "complex-condition-match-to-string operator or unmatch")
  (expect nil
    (bts:complex-condition-match-to-string
     "ahofuge"
     (bts:complex-condition-compile "hoge|fuga")))
  (desc "complex-condition-match-to-string operator and match")
  (expect t
    (bts:complex-condition-match-to-string
     "afugahogege"
     (bts:complex-condition-compile "hoge&fuga")))
  (desc "complex-condition-match-to-string operator and unmatch")
  (expect nil
    (bts:complex-condition-match-to-string
     "ahogege"
     (bts:complex-condition-compile "hoge&fuga")))
  (desc "complex-condition-match-to-string single list match")
  (expect t
    (bts:complex-condition-match-to-string
     "abaroge"
     (bts:complex-condition-compile "( hoge & fuga ) | bar")))
  (desc "complex-condition-match-to-string single list unmatch")
  (expect nil
    (bts:complex-condition-match-to-string
     "ahogege"
     (bts:complex-condition-compile "( hoge & fuga ) | bar")))
  (desc "complex-condition-match-to-string nested list match 1")
  (expect t
    (bts:complex-condition-match-to-string
     "ohogebaroge"
     (bts:complex-condition-compile "(hoge & (fuga|bar)) | (foo&baz)")))
  (desc "complex-condition-match-to-string nested list match 2")
  (expect t
    (bts:complex-condition-match-to-string
     "bazofooooo"
     (bts:complex-condition-compile "(hoge & (fuga|bar)) | (foo&baz)")))
  (desc "complex-condition-match-to-string nested list unmatch")
  (expect nil
    (bts:complex-condition-match-to-string
     "barofooooo"
     (bts:complex-condition-compile "(hoge & (fuga|bar)) | (foo&baz)")))
  )

(expectations
  (desc "complex-condition-match-to-list operator or match")
  (expect t
    (bts:complex-condition-match-to-list
     '("hoge")
     (bts:complex-condition-compile "aaa | bbb | foo | hoge | fuga")))
  (desc "complex-condition-match-to-list operator and match")
  (expect t
    (bts:complex-condition-match-to-list
     '("hoge" "bbb" "ccc" "foo" "aaa" "fuga")
     (bts:complex-condition-compile "aaa & bbb & foo & hoge & fuga")))
  (desc "complex-condition-match-to-list operator and unmatch")
  (expect nil
    (bts:complex-condition-match-to-list
     '("hoge" "bbb" "ccc" "bar" "aaa" "fuga")
     (bts:complex-condition-compile "aaa & bbb & foo & hoge & fuga")))
  (desc "complex-condition-match-to-list nested list match")
  (expect t
    (bts:complex-condition-match-to-list
     '("foo" "hoge" "aaa" "bar" "bbb")
     (bts:complex-condition-compile "(hoge & (fuga|bar)) | (foo&baz)")))
  (desc "complex-condition-match-to-list nested list unmatch")
  (expect nil
    (bts:complex-condition-match-to-list
     '("foo" "ahoge" "aaa" "bar" "bbb")
     (bts:complex-condition-compile "(hoge & (fuga|bar)) | (foo&baz)")))
  )


# ポートフォリオ

簡単に制作物をまとめました。

### 制作物

### OKASHI WARS

友人含めて4人のグループで、見下ろし型の2人対戦アクションゲームOKASHI WARSを制作しました。
文化祭の出店で展示する形だったので公に公開されてはいませんが、ゲームとして完成させました。
企画、作画、UI設計などを他3人が行い、技術的な方針決定と一部UI部分を除いた概ねのコーディングを私が行いました。

主に工夫した点は名前空間による役割の分離です。
クリーンアーキテクチャやMVCモデルを参考にしながらも、ゲームであることと、そこまで巨大なプロジェクトではないことを考慮して適切な依存関係の設計を行いました。
具体的には、ゲームAPIに依存しない「Logic層」、Logic層とゲームの描画APIに依存する「View層」、Logic層やView層、ユーザーからの入力に依存する「Interface層」の3つに分離しました。

また、格闘ゲームやアクションゲームに見られるフレーム単位での攻撃判定の有効化などを、
エディタ拡張によって以下のような簡単なスクリプト言語で記述できるようにし、
プログラミング経験の少ないメンバーも直感的に攻撃内容を記述できるようにしました。

```
PlayAnimation chocolate_normal
Wait 4
PlaySE swing
Wait 8
EnableHitbox normal
EnableHurtbox normal
Wait 9
DisableHitbox normal
Wait 14
DisableHurtbox normal
FinishAnimation
```

[デモ動画](./OKASHI_WARS_demo.mp4)

### 限定継続を含んだラムダ計算のインタプリタ
現在行っている専攻研究の関係で、Haskellで開発しました。

論文で記述される形式的な定義を、以下ソースコードのようになるべくそのまま書き下せるような工夫を行いました。
GitHubで[ソースコード](https://github.com/shota-kori/delim-cont-lambda)とWebブラウザで実行できる[デモ](https://shota-kori.github.io/delim-cont-lambda/html/)を公開しています。

```haskell
-- β簡約の実装
betaSimpSimple :: Expr -> Maybe Expr
betaSimpSimple (App (Val (Abs nm e)) after) = Just $ subst nm after e
betaSimpSimple (App (Val (Builtin (BinOp op))) (Val v@(VInt _))) =
    Just $ Val $ Builtin $ PartialBinOp op v
betaSimpSimple (App (Val (Builtin (PartialBinOp op (VInt n)))) (Val (VInt m))) =
    case op of
        Eq -> Just $ Val $ VBool $ n == m
        _ -> Just $ Val $ VInt $ (case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)) n m
betaSimpSimple (Let nm e1 e2) = Just $ subst nm e1 e2
betaSimpSimple (IfElse (Val (VBool True)) e2 _) = Just e2
betaSimpSimple (IfElse (Val (VBool False)) _ e3) = Just e3
betaSimpSimple _ = Nothing

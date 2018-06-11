package inference

import org.scalatest.FunSpec

class TypeCheckerTest extends FunSpec {
  describe("Addition test") {
    it("simple addition") {
      //<add> <const> 1 <const> 1
      val expected = IntType("a")
      val env = TypeChecker
      val addition = Add(Const(1), Const(1))
      val result = env.infer(addition, Map())
      assert(expected == result)
    }

    it("complex addition") {
      //<add>
      //  <add> <const> 1 <const> 1
      //  <add> <const> 2 <const> 2
      val expected = IntType("a")
      val env = TypeChecker
      val addition1 = Add(Const(1), Const(1))
      val addition2 = Add(Const(2), Const(2))
      val result = env.infer(Add(addition1, addition2), Map())
      assert(expected == result)
    }

    it("even more complex addition") {
      //<add>
      //  <add>
      //    <add> <const> 1 <const> 1
      //    <add> <const> 2 <const> 2
      //  <add>
      //    <add> <const> 1 <const> 1
      //    <add> <const> 2 <const> 2
      val expected = IntType("a")
      val env = TypeChecker
      val addition1 = Add(Const(1), Const(1))
      val addition2 = Add(Const(2), Const(2))
      val l = Add(addition1, addition2)
      val r = Add(addition2, addition1)
      val result = env.infer(Add(l, r), Map())
      assert(expected == result)
    }

    describe("Subtraction test") {
      it("simple subtraction") {
        //<sub> <const> 2 <const> 1
        val expected = IntType("a")
        val env = TypeChecker
        val sub = Sub(Const(2), Const(1))
        val result = env.infer(sub, Map())
        assert(expected == result)
      }
    }

    describe("Multiplication test") {
      it("simple multiplication") {
        //<mul> <const> 2 <const> 2
        val expected = IntType("a")
        val env = TypeChecker
        val mul = Mul(Const(2), Const(2))
        val result = env.infer(mul, Map())
        assert(expected == result)
      }
    }

    describe("Division test") {
      it("simple division") {
        //<diff> <const> 4 <const> 2
        val expected = IntType("a")
        val env = TypeChecker
        val diff = Diff(Const(4), Const(2))
        val result = env.infer(diff, Map())
        assert(expected == result)
      }
    }

    describe("Equality test") {
      it("Int equality true") {
        //<eq> <const> 2 <const> 2
        val expected = IntType("a")
        val env = TypeChecker
        val eq = Eq(Const(2), Const(2))
        val result = env.infer(eq, Map())
        assert(expected == result)
      }
    }

    describe("If test") {
      it("correct if") {
        //<if> <eq> <const> 2 <add> <const> 1 <const> 1
        //  <add> <const> 1 <const> 1
        //  <mul> <const> 2 <const> 2
        val expected = IntType("a")
        val env = TypeChecker
        val if_cond = Eq(Const(2), Add(Const(1), Const(1))) //true
        val branch_then = Add(Const(1), Const(1)) //2
        val branch_else = Mul(Const(2), Const(2)) //4
        val if_stat = If(if_cond, branch_then, branch_else)
        val result = env.infer(if_stat, Map())
        assert(expected == result)
      }

      it("incorrect if") {
        //the branches return different types
        val env = TypeChecker
        val if_cond = Eq(Const(2), Add(Const(1), Const(1)))
        // 1 + 1
        // IntType(a)
        val branch_then = Add(Const(1), Const(1))
        // (\x y -> x + y) 1
        // FuncType(a -> a)
        val branch_else = Apply(Lambda(List(Val("x"), Val("y")), Add(Val("x"), Val("y"))), List(Const(1)))

        val if_stat = If(if_cond, branch_then, branch_else)

        val thrown = intercept[ifBranchesException] {
          env.infer(if_stat, Map())
        }
        assert(thrown.getMessage == "Both branches return something different")
      }

    }

    describe("Equality test") {
      it("Int equality") {
        //<eq> <const> 2 <const> 2
        val expected = IntType("a")
        val env = TypeChecker
        val eq = Eq(Const(2), Const(2))
        val result = env.infer(eq, Map())
        assert(expected == result)
      }
    }


      describe("Val decl test") {
        it("simple val decl") {
          //<val-decl> "x" <const> 42 <mul> <val> "x" <const> 42
          val expected = IntType("a")
          val env = TypeChecker
          val val_decl = ValDecl(Map(Val("x") -> Const(42)), Mul(Val("x"), Const(42)))
          val result = env.infer(val_decl, Map())
          assert(expected == result)
        }

        it("shadow val decl") {
          val expected = IntType("a")
          val env = TypeChecker
          val function = Add(Val("x"), Val("y"))
          //exprShadow = Let (Var "x") (Const 1) (Var "y") (Const 2)
          //                Let (Var "x") (Const 3)
          //                  in x + y
          val shadow_val = ValDecl(Map(Val("y") -> Const(3)), function)
          val exprShadow = ValDecl(Map(Val("x") -> Const(1), Val("y") -> Const(2)), shadow_val)
          val result = env.infer(exprShadow, Map())

          assert(expected == result)
        }
      }

      describe("empty val-decl list") {
        it("check exception") {
          val env = TypeChecker
          val thrown = intercept[emptyListException] {
            env.infer(ValDecl(Map(), Val("x")), Map())
          }
          assert(thrown.getMessage == "Variable list in val-decl should not be empty")
        }
      }

    describe("Lambda apply test"){
      it("simple lambda apply -- int return"){
        val expected = IntType("a")
        val env = TypeChecker
        // (\x y -> x * y) 6 7
        val function = Mul(Val("x"), Val("y"))
        val lam = Lambda(List(Val("x"), Val("y")), function)
        val app = Apply(lam, List(Const(6), Const(7)))
        val result = env.infer(app, Map())
        assert(expected == result)
      }

      it("currying -- int return"){
        val expected = IntType("a")
        val env = TypeChecker
        // (\x -> (\y -> x * y ) 6) 7

        val function = Mul(Val("x"), Val("y"))
        val lam = Lambda(List(Val("x")), Lambda(List(Val("y")), function))
        val app = Apply(Apply(lam, List(Const(6))), List(Const(7)))
        val result = env.infer(app, Map())

        assert(expected == result)
      }

      it("lambda identity function"){
        val expected = FuncType("a -> a")
        val env = TypeChecker
        // \x -> x
        val lam = Lambda(List(Val("x")), Val("x"))
        val result = env.infer(lam, Map())
        assert(expected == result)
      }

      it("lambda simple -- function return"){
        val expected = FuncType("a -> a")
        val env = TypeChecker
        // (\x y -> x * y) 6
        val function = Mul(Val("x"), Val("y"))
        val lam = Lambda(List(Val("x"), Val("y")), function)
        val app = Apply(lam, List(Const(6)))
        val result = env.infer(app, Map())
        assert(expected == result)
      }

      it("lambda complex -- function return"){
        val expected = FuncType("a -> a -> a -> a")
        val env = TypeChecker
        // (\x y z t -> x * y + z - t) 6
        val function = Sub(Add(Mul(Val("x"), Val("y")), Val("z")), Val("t"))
        val lam = Lambda(List(Val("x"), Val("y"), Val("z"), Val("t")), function)
        val app = Apply(lam, List(Const(6)))
        val result = env.infer(app, Map())
        assert(expected == result)
      }
    }
  }
}



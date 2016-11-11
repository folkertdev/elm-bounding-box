module Laws exposing (leftIdentity, rightIdentity, associativity, commutativity, idempotence)

import Fuzz exposing (Fuzzer, tuple, tuple3)
import Test exposing (Test, fuzz)
import Expect


associativity : (a -> a -> a) -> Fuzzer a -> Test
associativity operation argumentFuzz =
    fuzz (tuple3 ( argumentFuzz, argumentFuzz, argumentFuzz )) "associativity"
        <| \( argument1, argument2, argument3 ) ->
            let
                left =
                    operation (operation argument1 argument2) argument3

                right =
                    operation argument1 (operation argument2 argument3)
            in
                Expect.equal left right


commutativity : (a -> a -> a) -> Fuzzer a -> Test
commutativity operation argumentFuzz =
    fuzz (tuple ( argumentFuzz, argumentFuzz )) "commutativity"
        <| \( argument1, argument2 ) ->
            let
                left =
                    operation argument1 argument2

                right =
                    operation argument2 argument1
            in
                Expect.equal left right


idempotence : (a -> b -> b) -> Fuzzer a -> Fuzzer b -> Test
idempotence operation leftFuzz rightFuzz =
    fuzz (tuple ( leftFuzz, rightFuzz )) "idempotence"
        <| \( argument1, argument2 ) ->
            let
                left =
                    operation argument1 argument2

                right =
                    operation argument1 (operation argument1 argument2)
            in
                Expect.equal left right


leftIdentity : (a -> b -> b) -> a -> Fuzzer b -> Test
leftIdentity operation e argumentFuzz =
    fuzz (argumentFuzz) "left identity"
        <| \argument ->
            Expect.equal (operation e argument) argument


rightIdentity : (b -> a -> b) -> a -> Fuzzer b -> Test
rightIdentity operation e argumentFuzz =
    fuzz (argumentFuzz) "right identity"
        <| \argument ->
            Expect.equal (operation argument e) argument

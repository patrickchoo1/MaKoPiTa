open Game

let () =
  () |> Game.StartScreen.setup |> Game.StartScreen.init |> Game.StartScreen.loop

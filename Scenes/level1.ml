open Raylib
open Level_def
open Makelevel
open Helpers

module Level1Data : LevelData = struct
  (* X:850, Y:500 *)

  let level_id = "Level1"

  let init_targets () =
    make_target (make_circle 30.0 (400.0, 225.0)) 0.009;
    make_target (make_circle 30.0 (400.0, 225.0)) 2.5;
    make_target (make_circle 30.0 (400.0, 225.0)) 3.9;
    make_target (make_circle 30.0 (100.0, 115.0)) 5.00;
    make_target (make_circle 30.0 (700.0, 115.0)) 7.00;
    make_target (make_circle 30.0 (600.0, 310.0)) 8.70;
    make_target (make_circle 30.0 (700.0, 115.0)) 10.00;
    make_target (make_circle 30.0 (100.0, 310.0)) 14.00;
    make_target (make_circle 30.0 (212.0, 335.0)) 16.50;
    make_target (make_circle 30.0 (200.0, 260.0)) 19.10;
    make_target (make_circle 30.0 (250.0, 310.0)) 20.00;
    make_target (make_circle 30.0 (340.0, 50.0)) 22.00;
    make_target (make_circle 30.0 (340.0, 50.0)) 24.80;
    make_target (make_circle 30.0 (100.0, 225.0)) 29.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 30.00;
    make_target (make_circle 30.0 (700.0, 225.00)) 31.00;
    make_target (make_circle 30.0 (700.0, 380.00)) 33.00;
    make_target (make_circle 30.0 (400.0, 325.00)) 35.50;
    make_target (make_circle 30.0 (400.0, 420.00)) 37.00;
    make_target (make_circle 30.0 (50.0, 30.00)) 39.50;
    make_target (make_circle 30.0 (50.0, 225.00)) 41.00;
    make_target (make_circle 30.0 (50.0, 420.00)) 42.00;
    make_target (make_circle 30.0 (50.0, 420.00)) 49.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 52.00;
    make_target (make_circle 30.0 (750.0, 410.00)) 55.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 57.50;
    make_target (make_circle 30.0 (400.0, 225.00)) 60.00;
    make_target (make_circle 30.0 (400.0, 180.00)) 63.00;
    make_target (make_circle 30.0 (700.0, 400.00)) 66.00;
    make_target (make_circle 30.0 (700.0, 390.00)) 68.50;
    make_target (make_circle 30.0 (700.0, 380.00)) 71.00;
    make_target (make_circle 30.0 (700.0, 370.00)) 74.00;
    make_target (make_circle 30.0 (700.0, 40.00)) 76.80;
    make_target (make_circle 30.0 (700.0, 225.00)) 82.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 85.00;
    make_target (make_circle 30.0 (100.0, 225.00)) 87.60;
    make_target (make_circle 30.0 (30.0, 30.00)) 90.00;
    make_target (make_circle 30.0 (100.0, 225.00)) 93.00;
    make_target (make_circle 30.0 (770.0, 420.00)) 95.80;
    make_target (make_circle 30.0 (770.0, 320.00)) 101.00;
    make_target (make_circle 30.0 (750.0, 320.00)) 104.00;
    make_target (make_circle 30.0 (720.0, 320.00)) 107.00;
    make_target (make_circle 30.0 (700.0, 320.00)) 111.00;
    make_target (make_circle 30.0 (680.0, 320.00)) 115.00;
    make_target (make_circle 30.0 (60.0, 400.00)) 117.50;
    make_target (make_circle 30.0 (60.0, 350.00)) 118.00;
    make_target (make_circle 30.0 (60.0, 300.00)) 118.70;
    make_target (make_circle 30.0 (400.0, 225.00)) 123.00;
    make_target (make_circle 30.0 (750.0, 410.00)) 125.50;
    make_target (make_circle 30.0 (400.0, 225.00)) 128.70;
    make_target (make_circle 30.0 (400.0, 225.00)) 131.3;
    make_target (make_circle 30.0 (400.0, 180.00)) 134.00;
    make_target (make_circle 30.0 (700.0, 400.00)) 137.00;
    make_target (make_circle 30.0 (700.0, 390.00)) 140.00;
    make_target (make_circle 30.0 (700.0, 380.00)) 142.8;
    make_target (make_circle 30.0 (30.0, 420.00)) 144.5;
    make_target (make_circle 30.0 (80.0, 380.00)) 144.8;
    make_target (make_circle 30.0 (100.0, 350.00)) 145.00;
    make_target (make_circle 30.0 (120.0, 330.00)) 147.5;
    make_target (make_circle 30.0 (140.0, 310.00)) 145.80;
    make_target (make_circle 30.0 (160.0, 290.00)) 148.00;
    make_target (make_circle 30.0 (180.0, 270.00)) 149.00;
    make_target (make_circle 30.0 (200.0, 250.00)) 152.00;
    make_target (make_circle 30.0 (200.0, 250.00)) 156.00;
    make_target (make_circle 30.0 (200.0, 260.00)) 158.50;
    make_target (make_circle 30.0 (700.0, 225.00)) 164.00;
    make_target (make_circle 30.0 (700.0, 380.00)) 167.00;
    make_target (make_circle 30.0 (770.0, 320.00)) 169.50;
    make_target (make_circle 30.0 (700.0, 380.00)) 172.50;
    make_target (make_circle 30.0 (30.0, 420.00)) 175.00;
    make_target (make_circle 30.0 (30.0, 420.00)) 178.00;
    make_target (make_circle 30.0 (80.0, 380.00)) 181.0;
    make_target (make_circle 30.0 (100.0, 350.00)) 183.5;
    make_target (make_circle 30.0 (400.0, 225.00)) 186.00;
    make_target (make_circle 30.0 (400.0, 225.00)) 189.00

  let music_path = "music/plantasia.mp3"
  let target_interval = 2.0
end

module Level1 = MakeLevel (Level1Data)

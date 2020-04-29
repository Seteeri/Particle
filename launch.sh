sudo pkill -f pil

pil -"scl 6" src/render/render.l -main + &
pil -"scl 6" src/input/input.l   -main + &
pil -"scl 6" src/ctrl/ctrl.l     -main + &
pil -"scl 6" src/worker/worker.l -main + &

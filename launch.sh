sudo pkill -f pil

pil -"scl 6" particle/render/render.l -main + &
pil -"scl 6" particle/ctrl/ctrl.l     -main + &
pil -"scl 6" particle/input/input.l   -main + &
pil -"scl 6" particle/worker/worker.l -main + &

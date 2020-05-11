let Walls = {'test': {}, 'train': {}, 'tilted': {}};

// ground of scene
const Bottom = wall(label='bottom', x=scene.w/2, y=scene.h - PROPS.bottom.h/2,
  w=scene.w, h=PROPS.bottom.h);

// base walls
let W1 = wall('w1_upLeft', 320, 100);
let W1_2 = wall('w1_2_upRight', 480, 100);
let W3 = wall('w3_rampLowInd', 414, 240, 175)
let W3_2 = wall('w3_2_rampLowInd', 386, 240, 175)
let W4 = wall('w4_upRight', 750, 240, 90);
// let W5 = wall('w5_base_ramp', 250+(PROPS.walls.w+25)/2, 225, W_BASE_RAMP)
let W5 = wall('w5_base_ramp', 320, 225, W_BASE_RAMP)
let W6 = wall('w6_upRight', 750, 240, 90)


makeRamp = function(angle, tilt_increase, wallLow, test=true){
  let overlap = OVERLAP_SHIFT[ "angle" + Math.abs(angle)];

  let pos = tilt_increase ? {shift_x: 1, w_low_x_edge: wallLow.bounds.max.x} :
    {shift_x: -1, w_low_x_edge: wallLow.bounds.min.x};

  // 1. sin(angle) = h/w_tillted and 2. h² + w_low² = ramp²
  let r = radians(Math.abs(angle))
  let ramp_width = Math.sqrt(Math.pow(100, 2) / (1 - Math.pow(Math.sin(r), 2)))

  let hMiddle = Math.sqrt(Math.pow(ramp_width/2, 2) -
    Math.pow(Math.cos(r)*ramp_width/2, 2));
  let ramp_y = wallLow.position.y - hMiddle;

  let ramp_x = pos.w_low_x_edge + pos.shift_x*ramp_width/2 - pos.shift_x*overlap
  let ramp = wall('ramp' + angle, ramp_x, ramp_y, ramp_width);
  pos.x_edge_w_top = tilt_increase ? ramp.bounds.max.x : ramp.bounds.min.x;

  let wallTop_y = ramp.position.y - hMiddle + PROPS.walls.h/2;

  let wallTop = wall(label = 'ramp_top' + angle,
    x = pos.x_edge_w_top + pos.shift_x * PROPS.walls.w/2 - pos.shift_x * overlap,
    y = wallTop_y - PROPS.walls.h/2
  );
  pos.x_ball = tilt_increase ? wallTop.bounds.min.x - 4 : wallTop.bounds.max.x + 4;
  let col_ball = test ? ball_colors.test : ball_colors.train[Math.abs(angle).toString()];
  let ball1 = ball(pos.x_ball, wallTop.bounds.min.y - PROPS.balls.radius,
    PROPS.balls.radius, 'ball1', col_ball);
  Body.setAngle(ramp, -pos.shift_x * r);
  return {'tilted': ramp, 'top': wallTop, 'ball': ball1}
}

rampElems = function(ramp_type, increase, base, horiz, test=true){
  let dir = horiz ? 'horizontal' : 'vertical'
  let angle = -1 * ANGLES[dir][ramp_type]
  return _.values(makeRamp(angle, increase, base, test))
}

seesaw = function(pos, height="high"){
  let kind = "seesaw_" + height;
  let stick = wall('stick', pos, scene.h - PROPS.bottom.h - PROPS[kind].stick.h / 2,
    PROPS[kind].stick.w, PROPS[kind].stick.h, {render: {fillStyle: cols.darkgrey}});

  let link = wall('link', pos, stick.bounds.min.y - PROPS[kind].link.h/2,
    PROPS[kind].link.w, PROPS[kind].link.h, {render: {fillStyle: cols.darkbrown}}
  );
  let skeleton = Body.create({'parts': [stick, link],
    'isStatic': true,
    'label': 'skeleton'
  });
  let def_ss = PROPS[kind];
  let y_plank = link.bounds.min.y - def_ss.plank.h/2;
  let opts = Object.assign({label:'plank', render:{fillStyle: cols.plank}}, OPTS.blocks);
  let plank = Bodies.rectangle(pos, y_plank, def_ss.plank.w, def_ss.plank.h, opts)

  var constraint = Constraint.create({
    pointA: {x: plank.position.x, y: plank.position.y},
    bodyB: plank,
    stiffness: 0.7,
    length: 0
  });
  return {stick, link, skeleton, plank, constraint}
}

W_IF_UP1 = wall('wall_ac_top', 550, 100),
W_IF_UP2 = wall('wall_ac_top', 100, 100),
W_IF_LOW = wall('wall_ac_low', 325, 290, W_BASE_RAMP)

// The first two list entries are respectively the bases for block1 and block2
Walls.test = {'independent': [[W1, W3], [W1_2, W3_2]], // tilted wall+ball added dep on prior
              'a_implies_c': [[W_IF_UP1, W_IF_LOW], [W_IF_UP2, W_IF_LOW]],
              'a_iff_c': []
              };

Walls.test.seesaw_trials = function(side_ramp, offset=PROPS.seesaw.d_to_walls){
  let data = side_ramp === "right" ? {x0: 175, w0: 0.6 * W_BASE_RAMP, w1: W_BASE_RAMP}
                                   : {x0: 275, w0: W_BASE_RAMP, w1: 0.6 * W_BASE_RAMP};
  let base0 = wall('wall_seesaw_left', data.x0, 240, data.w0);
  let pos = base0.bounds.max.x + PROPS.seesaw_high.plank.w/2 + offset;
  let objs = seesaw(pos);
  let base1 = wall('wall_seesaw_right',
    objs.plank.bounds.max.x + offset + data.w1/2, base0.position.y, data.w1);
  let walls = [base0, base1].concat([objs.skeleton]);
  return {'walls': walls, 'dynamic': [objs.plank, objs.constraint]}
}


//// Elements for TRAINING TRIALS //////
let W8 = wall('w8_middle_left', 0.3 * scene.w, scene.h/3);
let W9 = wall('w9_middle_right', (3/4) * scene.w, scene.h/3);
let W10 = wall('w10_right_low', (2/3) * scene.w, (3/4) * scene.h);
let W11 = wall('w11_top', 400, 120, W_BASE_RAMP)
let W12 = wall('w12_middle', 400, 230, W_BASE_RAMP)
let W13 = wall('w13_down', 400, 350, W_BASE_RAMP)
Walls.train.uncertain = [[W8, W9, W10], [W11, W12, W13]]

Walls.train.a_implies_c = function(){
  return [wall('wall_ac_top', 580, 150, W_BASE_RAMP),
          wall('wall_ac_low', 390, 320)];
}

Walls.train.seesaw_trials = function(){
  let objs = seesaw(scene.w/2)
  let angle = 45;
  let ramp = wall('ramp', 200, 175, Math.pow(10,2)*Math.sqrt(2), PROPS.walls.h,
    OPTS['ramp'+angle]);
  Body.setAngle(ramp, radians(angle));
  // first two elements are bases for blocks!
  let walls = [wall('wallTopLeft', 100, 125, 100),
               wall('wall_seesaw_right', 600, 240, 175), ramp].concat([objs.skeleton]);
  return {'walls': walls, 'dynamic': [objs.plank, objs.constraint]}
}

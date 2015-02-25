function animate() {
    return renderer.render(stage);
};
var w = 1024;
var h = 768;
var stage = new PIXI.Stage(0xffffff, true);
var renderer = PIXI.autoDetectRenderer(w, h);
document.body.appendChild(renderer.view);
var background = PIXI.Sprite.fromImage('img/background.jpg');
stage.addChild(background);
var balloon = PIXI.Texture.fromImage('img/balloon_red.png');
var clickified = PIXI.Texture.fromImage('img/balloon_red_clicked.png');
var bbclicked = new PIXI.Sprite(clickified);
var bb1 = new PIXI.Sprite(balloon);
var bb2 = new PIXI.Sprite(balloon);
var bb3 = new PIXI.Sprite(balloon);
function interactive_setup(thing) {
    thing.mouseover = function (data) {
        return !this.is_swapped ? this.scale.set(1.1, 1.1) : null;
    };
    thing.mouseout = function (data) {
        return !this.is_swapped ? this.scale.set(1, 1) : null;
    };
    return thing.click = function (data) {
        if (this.is_swapped) {
            this.is_swapped = null;
            this.scale.set(1, 1);
            return this.setTexture(balloon);
        } else {
            this.is_swapped = 1;
            this.scale.set(1.4, 1.4);
            return this.setTexture(clickified);
        };
    };
};
bb1.anchor.x = 0.5;
bb1.anchor.y = 0.5;
bb1.position.x = 115;
bb1.position.y = 190;
stage.addChild(bb1);
interactive_setup(bb1);
bb2.anchor.x = 0.5;
bb2.anchor.y = 0.5;
bb2.position.x = 280;
bb2.position.y = 190;
stage.addChild(bb2);
interactive_setup(bb2);
bb3.anchor.x = 0.5;
bb3.anchor.y = 0.5;
bb3.position.x = 450;
bb3.position.y = 190;
stage.addChild(bb3);
interactive_setup(bb3);
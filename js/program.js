var scene = new THREE.Scene();
var width = window.innerWidth;
var height = window.innerHeight;
var aspectRatio = width / height;
var camera = new THREE.PerspectiveCamera(45, aspectRatio, 1, 1000);
//var camera = new THREE.OrthographicCamera(width / - 2, width / 2, height / 2, height / - 2, 1, 1000);

var renderer = new THREE.WebGLRenderer();
renderer.setSize(width, height);
document.body.appendChild(renderer.domElement);

var uniforms = { 
    u_time: { type: "f", value: 0.0 },
    u_aspectRatio: { type: "f", value: aspectRatio },
    u_resolution: { type: "v2", value: new THREE.Vector2(width, height) }
};
        
var geometry = new THREE.PlaneGeometry(aspectRatio, 1);
var material = new THREE.ShaderMaterial({
    uniforms: uniforms,
    attributes: null, 
    fragmentShader: document.getElementById('fragmentShader').textContent
});
var plane = new THREE.Mesh(geometry, material);
scene.add(plane);

camera.position.z = 1.2;

function render() {
    uniforms.u_time.value = timer.getElapsedTime();
	renderer.render(scene, camera);
	requestAnimationFrame(render);
}
var timer = new THREE.Clock(true);
render();

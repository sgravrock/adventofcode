import {parseInput, showCave, showChanged, run} from './lib.mjs';

let animations = document.querySelector('form').animations.value;

document.querySelector('form').addEventListener('change', function(e) {
	if (e.target.name === 'animations') {
		animations = e.target.value;
	}
});

document.querySelector('button').addEventListener('click', async function(e) {
	e.preventDefault();
	const cave = parseInput(document.querySelector('textarea').value);
	const table = document.querySelector('table');

	showCave(cave, table);
	const startTime = new Date().getTime();
	let frameCount = 0;
	let changedSinceLastFrame = [];

	const n = await run(cave, async (changed) => {
		frameCount++;

		if (animations === 'none') {
			return;
		}

		changedSinceLastFrame = [...changedSinceLastFrame, ...changed];
		let animationRate;

		if (animations === 'all') {
			animationRate = 1;
		} else {
			animationRate = parseInt(animations, 10);
		}

		if (frameCount % animationRate === 0) {
			showChanged(cave, table, changedSinceLastFrame);
			changedSinceLastFrame = [];
			await new Promise(res => requestAnimationFrame(res));
		} else if (animationRate > 200 && frameCount % 200 === 0) {
			// Tick anyway so the browser doesn't kill us
			await new Promise(res => requestAnimationFrame(res));
		}
	});

	showCave(cave, table);
	alert('Answer: ' + n + '. Time: ' + ((new Date().getTime() - startTime) / 1000) + 's');
});

import {parseInput, showCave, showChanged, run} from './lib.mjs';

document.querySelector('button').addEventListener('click', async function(e) {
	e.preventDefault();
	const cave = parseInput(document.querySelector('textarea').value);
	const table = document.querySelector('table');
	const showAnimations = document.querySelector('input[type=checkbox]').checked;
	showCave(cave, table);
	const startTime = new Date().getTime();
	let lastAnimationTime = startTime;

	const n = await run(cave, async (changed) => {
		if (showAnimations) {
			showChanged(cave, table, changed);
			await new Promise(res => setTimeout(res));
		}
	});

	if (!showAnimations) {
		showCave(cave, table);
	}

	alert('Answer: ' + n + '. Time: ' + ((new Date().getTime() - startTime) / 1000) + 's');
});

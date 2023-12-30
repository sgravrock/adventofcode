import {ArenaView, Arena, Rock} from '../lib.mjs';

describe('ArenaView', function() {
	it('expands to accomodate a new rock', function() {
		const table = createTable();
		const subject = new ArenaView(table);
		const arena = new Arena();

		arena.addRock([['#','#'],['#','#']]); 
		subject.update(arena);

		const rows = table.querySelectorAll('tr');
		const cellTexts = Array.from(rows).map(row => {
			return Array.from(row.cells).map(cell => cell.textContent);
		});

		expect(rows.length).withContext('# rows').toEqual(6);
		expect(cellTexts[0]).withContext('row 0')
			.toEqual(['|', '.', '.', '#', '#', '.', '.', '.', '|']);
		expect(cellTexts[1]).withContext('row 1')
			.toEqual(['|', '.', '.', '#', '#', '.', '.', '.', '|']);
		expect(cellTexts[2]).withContext('row 2')
			.toEqual(['|', '.', '.', '.', '.', '.', '.', '.', '|']);
		expect(cellTexts[3]).withContext('row 3')
			.toEqual(['|', '.', '.', '.', '.', '.', '.', '.', '|']);
		expect(cellTexts[4]).withContext('row 4')
			.toEqual(['|', '.', '.', '.', '.', '.', '.', '.', '|']);
		expect(cellTexts[5]).withContext('row 5')
			.toEqual(['+', '-', '-', '-', '-', '-', '-', '-', '+']);
	});
	
	function createTable() {
		const table = document.createElement('table');
		table.innerHTML = '<tbody><tr><td>+</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>+</td></tbody';
		return table;
	}
});

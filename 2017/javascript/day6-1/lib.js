function maxRedistributions(banks) {
	const seen = new BankSet();
	seen.add(banks);

	while (true) {
		banks = redistributeBlocks(banks);

		if (seen.contains(banks)) {
			return seen.size();
		}

		seen.add(banks);
	}
}

function redistributeBlocks(banks) {
	banks = banks.slice();
	const startIx = sourceBankIx(banks);
	const nblocks = banks[startIx];
	banks[startIx] = 0;

	for (let i = 1; i <= nblocks; i++) {
		banks[(startIx + i) % banks.length]++;
	}

	return banks;
}

function sourceBankIx(banks) {
	let max = -1, maxIx = -1;

	for (let i = 0; i < banks.length; i++) {
		if (banks[i] > max) {
			max = banks[i];
			maxIx = i;
		}
	}

	return maxIx;
}

class BankSet {
	constructor() {
		this._els = [];
	}

	add(el) {
		if (!this.contains(el)) {
			this._els.push(el);
		}
	}

	contains(el) {
		const i = this._els.find(x => equalArrays(el, x));
		return i !== undefined;
	}

	size() {
		return this._els.length;
	}
}

function equalArrays(a, b) {
	if (a.length !== b.length) {
		return false;
	}

	for (let i = 0; i < a.length; i++) {
		if (a[i] !== b[i]) {
			return false;
		}
	}

	return true;
}

module.exports = {
	maxRedistributions: maxRedistributions,
	redistributeBlocks: redistributeBlocks,
	sourceBankIx: sourceBankIx,
	BankSet: BankSet,
};

let puppeteer = require('puppeteer-core');

const facultad = 'FING'
const carrera = 'INGENIERIA ELECTRICA'
// const carrera = 'INGENIERIA EN COMPUTACION'
const planYear = '1997'

async function main() {
	let browser = await puppeteer.launch({
		executablePath: '/usr/bin/chromium',
		defaultViewport: {
			width: 1024,
			height: 800,
		}
	})
	let page = await browser.newPage()
	await page.goto('https://bedelias.udelar.edu.uy', {waitUntil: 'networkidle2'})
	await page.click('#Menu\\:button_menu_button')
	await (await page.$$('.ui-menuitem-link > .ui-menuitem-text'))[1].click()
	await page.waitForSelector('#PageContent')
	await page.evaluate(clickFacultad, facultad)
	await page.waitFor(1000)
	await page.evaluate(clickCarrera, carrera, planYear)
	await page.waitFor(2000)
	let plan = await page.evaluate(getPlan)
	process.stdout.write(JSON.stringify(plan, null, 2))
	await page.screenshot({path: 'bedelias.png'})
	await browser.close();
}


function clickFacultad(facultadCode) {
	let pageContent = document.querySelector('#PageContent')
	let h3s = pageContent.querySelectorAll('h3')
	for (let h3 of h3s) {
		h3.click()
		setTimeout(() => {
			let rows = pageContent.querySelectorAll('.ui-datatable-selectable')
			for (let row of rows) {
				let code = row.innerText.split(' - ')[0]
				if (code == facultadCode) {
					row.children[0].click()
				}
			}
		}, 500)
	}
}


function listCarreras() {
	let pageContent = document.querySelector('#PageContent')
	let table = pageContent.querySelector('.ui-datatable')
	let trs = table.querySelectorAll('tr.ui-widget-content')
	let carreras = []
	for (let tr of trs) {
		let tds = tr.children
		let button = tds[0]
		let name = tds[1].innerText
		carreras.push(name)
	}
	return carreras
}


function clickCarrera(carrera, plan) {
	let pageContent = document.querySelector('#PageContent')
	let table = pageContent.querySelector('.ui-datatable')
	let trs = table.querySelectorAll('tr.ui-widget-content')
	let carreras = []
	for (let tr of trs) {
		let tds = tr.children
		let button = tds[0].firstChild
		let name = tds[1].innerText
		if (name == carrera) {
			button.click()
			setTimeout(() => {
				let planes = tr.nextSibling.querySelector('tbody.ui-datatable-data')
				let trs = planes.querySelectorAll('tr')
				for (let tr of trs) {
					let tds = tr.children
					let year = tds[0].innerText
					let name = tds[1].innerText
					let current = tds[2].innerText
					let button = tds[3].firstChild
					if (year == plan)
						button.click()
				}
			}, 500)
		}
	}
}


function getPlan() {
	function getTree(node) {
		var $node = $(node)
		var s = $node.children('.ui-treenode-content').text().trim()
		var t = $node.data().nodetype
		var cs = []
		$node
			.children('.ui-treenode-children')
			.children('.ui-treenode').each(function (i, el) {
				cs.push(getTree(el))
			})

		if (cs.length == 1 && cs[0].type == 'etiqueta') {
			return {content: s, children: cs[0].children, type: t}
		}
		return {content: s, children: cs, type: t}
	}

	return getTree($('#datosComposicion .ui-treenode')[0])
}


main()
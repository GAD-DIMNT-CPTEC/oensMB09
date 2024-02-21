importScripts("https://cdn.jsdelivr.net/pyodide/v0.24.1/full/pyodide.js");

function sendPatch(patch, buffers, msg_id) {
  self.postMessage({
    type: 'patch',
    patch: patch,
    buffers: buffers
  })
}

async function startApplication() {
  console.log("Loading pyodide!");
  self.postMessage({type: 'status', msg: 'Loading pyodide'})
  self.pyodide = await loadPyodide();
  self.pyodide.globals.set("sendPatch", sendPatch);
  console.log("Loaded!");
  await self.pyodide.loadPackage("micropip");
  const env_spec = ['https://cdn.holoviz.org/panel/wheels/bokeh-3.3.4-py3-none-any.whl', 'https://cdn.holoviz.org/panel/1.3.8/dist/wheels/panel-1.3.8-py3-none-any.whl', 'pyodide-http==0.2.1', 'numpy', 'pandas']
  for (const pkg of env_spec) {
    let pkg_name;
    if (pkg.endsWith('.whl')) {
      pkg_name = pkg.split('/').slice(-1)[0].split('-')[0]
    } else {
      pkg_name = pkg
    }
    self.postMessage({type: 'status', msg: `Installing ${pkg_name}`})
    try {
      await self.pyodide.runPythonAsync(`
        import micropip
        await micropip.install('${pkg}');
      `);
    } catch(e) {
      console.log(e)
      self.postMessage({
	type: 'status',
	msg: `Error while installing ${pkg_name}`
      });
    }
  }
  console.log("Packages loaded!");
  self.postMessage({type: 'status', msg: 'Executing code'})
  const code = `
  
import asyncio

from panel.io.pyodide import init_doc, write_doc

init_doc()

#!/usr/bin/env python
# coding: utf-8

# In[1]:


import panel as pn
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

pn.extension(sizing_mode='stretch_width')


# In[2]:


exps = ['gnu_singularity_m128p_p64p', 'gnu_egeon_m128p_p64p', 'intel_egeon_m128p_p64p'] 
prods = ['spread', 'cluster']
dates = pd.date_range(start='2020-12-01 00:00', end='2020-12-15 00:00', freq='1D')
products = ['spread', 'spaguetti', 'cluster', 'probability', 'probagr', 'perturbations', 'chievol']

vars_spread = ['psnm', 'geop250', 'geop500', 'temp850']
vars_spaguetti = ['geop500', 'temp850', 'temp925', 'temp1000']
vars_cluster = ['precpsnm1000', 'precvento250', 'precvento925', 'temp700', 'temp850', 'temp925', 'temp1000', 'tems1000', 'zgeo200', 'zgeo500']
vars_probagr = ['semana 1', 'semana 2', 'semana 3']
vars_perturbations = ['temp250', 'temp500', 'temp850', 'uvel250', 'uvel500', 'uvel850', 'vvel250', 'vvel500', 'vvel850']

regs_spaguetti = ['gl', 'as']

prod = pn.widgets.Select(name='Produto', options=products)
date = pn.widgets.Select(name='Data', options=list(dates))
exp = pn.widgets.Select(name='Experimento', options=list(exps))
var_spread = pn.widgets.Select(name='Variável', options=list(vars_spread))
var_spaguetti = pn.widgets.Select(name='Variável', options=list(vars_spaguetti))
var_cluster = pn.widgets.Select(name='Variável', options=list(vars_cluster))
var_probagr = pn.widgets.Select(name='Variável', options=list(vars_probagr))
var_perturbations = pn.widgets.Select(name='Variável', options=list(vars_perturbations))

reg_spaguetti = pn.widgets.Select(name='Região', options=list(regs_spaguetti))

proxy = 'https://corsproxy.io/?'
#proxy = 'https://images.weserv.nl/?url='

url_fmt_spread = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/{var}{datei}{datef}-fs8.png')
url_fmt_spaguetti = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/spt{reg}{var}{datei}{datef}-fs8.png')
url_fmt_cluster = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/{prod}{var}{datei}{datef}-fs8.png')
url_fmt_probability = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/prec{datei}{datef}-fs8.png')
url_fmt_probagr = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/prec_agric_large{num}-fs8.png')
url_fmt_perturbations = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/perturbations{var}_{datei}{datei}-fs8.png')
url_fmt_chievol = (proxy + 'http://ftp1.cptec.inpe.br/pesquisa/das/carlos.bastarz/oensMB09/exps/{exp}/prod/{prod}/{datei}/chi_evol{datei}{datef}-fs8.png')

@pn.depends(date, var_spread)
def fig_spread(date, var_spread):

    datei = date
    datef = date
    
    image_rows = []
        
    for i in np.arange(0,384,24):
        image_row = pn.Row()
        fcst_hour = f"Fct. {i:03d}h"
        datef = datei + timedelta(hours=int(i))
        url1 = url_fmt_spread.format(exp='gnu_singularity_m128p_p64p', var=var_spread, prod='spread', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url2 = url_fmt_spread.format(exp='gnu_egeon_m128p_p64p', var=var_spread, prod='spread', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url3 = url_fmt_spread.format(exp='intel_egeon_m128p_p64p', var=var_spread, prod='spread', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=490)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=490)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=490)))
        image_rows.append((fcst_hour, image_row))
    
    tabs = pn.Tabs(*image_rows, align='center', dynamic=True)
    player = pn.widgets.Player(end=len(image_rows) - 1, loop_policy='loop', align='center')
    player.jslink(tabs, value='active', bidirectional=True)

    return pn.Column(tabs, player)
      
@pn.depends(date, var_spaguetti, reg_spaguetti)
def fig_spaguetti(date, var_spaguetti, reg_spaguetti):

    datei = date
    datef = date
    
    image_rows = []
        
    for i in np.arange(0,384,24):
        image_row = pn.Row()
        fcst_hour = f"Fct. {i:03d}h"
        datef = datei + timedelta(hours=int(i))
        url1 = url_fmt_spaguetti.format(exp='gnu_singularity_m128p_p64p', var=var_spaguetti, reg=reg_spaguetti, prod='spaguetti', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url2 = url_fmt_spaguetti.format(exp='gnu_egeon_m128p_p64p', var=var_spaguetti, reg=reg_spaguetti, prod='spaguetti', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url3 = url_fmt_spaguetti.format(exp='intel_egeon_m128p_p64p', var=var_spaguetti, reg=reg_spaguetti, prod='spaguetti', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=490)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=490)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=490)))
        image_rows.append((fcst_hour, image_row))
    
    tabs = pn.Tabs(*image_rows, align='center', dynamic=True)
    player = pn.widgets.Player(end=len(image_rows) - 1, loop_policy='loop', align='center')
    player.jslink(tabs, value='active', bidirectional=True)

    return pn.Column(tabs, player)
    
@pn.depends(date, var_cluster)
def fig_cluster(date, var_cluster):

    datei = date
    datef = date
    
    image_rows = []
    
    for i in np.arange(0,384,24):
        image_row = pn.Row()
        fcst_hour = f"Fct. {i:03d}h"
        datef = datei + timedelta(hours=int(i))
        url1 = url_fmt_cluster.format(exp='gnu_singularity_m128p_p64p', var=var_cluster, prod='cluster', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url2 = url_fmt_cluster.format(exp='gnu_egeon_m128p_p64p', var=var_cluster, prod='cluster', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url3 = url_fmt_cluster.format(exp='intel_egeon_m128p_p64p', var=var_cluster, prod='cluster', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=490)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=490)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=490)))
        image_rows.append((fcst_hour, image_row))
    
    tabs = pn.Tabs(*image_rows, align='center', dynamic=True)
    player = pn.widgets.Player(end=len(image_rows) - 1, loop_policy='loop', align='center')
    player.jslink(tabs, value='active', bidirectional=True)

    return pn.Column(tabs, player)    
    
@pn.depends(date)
def fig_probability(date):

    datei = date
    datef = date
    
    image_rows = []
    
    for i in np.arange(24,384,24):
        image_row = pn.Row()
        fcst_hour = f"Fct. {i:03d}h"
        datef = datei + timedelta(hours=int(i))
        url1 = url_fmt_probability.format(exp='gnu_singularity_m128p_p64p', prod='probability', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url2 = url_fmt_probability.format(exp='gnu_egeon_m128p_p64p', prod='probability', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        url3 = url_fmt_probability.format(exp='intel_egeon_m128p_p64p', prod='probability', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=590)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=590)))
        image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=590)))
        image_rows.append((fcst_hour, image_row))
    
    tabs = pn.Tabs(*image_rows, align='center', dynamic=True)
    player = pn.widgets.Player(end=len(image_rows) - 1, loop_policy='loop', align='center')
    player.jslink(tabs, value='active', bidirectional=True)

    return pn.Column(tabs, player)      
    
@pn.depends(date, var_probagr)
def fig_probagr(date, var_probagr):

    datei = date
    
    if var_probagr == 'semana 1': num = '5'
    if var_probagr == 'semana 2': num = '10'
    if var_probagr == 'semana 3': num = '15'
    
    image_row = pn.Row()
    url1 = url_fmt_probagr.format(exp='gnu_singularity_m128p_p64p', prod='probagr', var=var_probagr, datei=datei.strftime('%Y%m%d%H'), num=num)
    url2 = url_fmt_probagr.format(exp='gnu_egeon_m128p_p64p', prod='probagr', var=var_probagr, datei=datei.strftime('%Y%m%d%H'), num=num)
    url3 = url_fmt_probagr.format(exp='intel_egeon_m128p_p64p', prod='probagr', var=var_probagr, datei=datei.strftime('%Y%m%d%H'), num=num)
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=495)))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=495)))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=495)))

    return pn.Column(image_row)      

@pn.depends(date, var_perturbations)
def fig_perturbations(date, var_perturbations):

    datei = date
    
    image_row = pn.Row()
    url1 = url_fmt_perturbations.format(exp='gnu_singularity_m128p_p64p', var=var_perturbations, prod='perturbations', datei=datei.strftime('%Y%m%d%H'))
    url2 = url_fmt_perturbations.format(exp='gnu_egeon_m128p_p64p', var=var_perturbations, prod='perturbations', datei=datei.strftime('%Y%m%d%H'))
    url3 = url_fmt_perturbations.format(exp='intel_egeon_m128p_p64p', var=var_perturbations, prod='perturbations', datei=datei.strftime('%Y%m%d%H'))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=495)))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=495)))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=495)))

    return pn.Column(image_row)   

@pn.depends(date)
def fig_chievol(date):

    datei = date
    datef = date + timedelta(hours=360)
    
    image_row = pn.Row()
    url1 = url_fmt_chievol.format(exp='gnu_singularity_m128p_p64p', prod='chievol', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
    url2 = url_fmt_chievol.format(exp='gnu_egeon_m128p_p64p', prod='chievol', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
    url3 = url_fmt_chievol.format(exp='intel_egeon_m128p_p64p', prod='chievol', datei=datei.strftime('%Y%m%d%H'), datef=datef.strftime('%Y%m%d%H'))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_singularity_m128p_p64p'), pn.pane.PNG(url1, width=595)))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='gnu_egeon_m128p_p64p'), pn.pane.PNG(url2, width=595)))
    image_row.append(pn.Column(pn.widgets.StaticText(name='Experimento', value='intel_egeon_m128p_p64p'), pn.pane.PNG(url3, width=595)))

    return pn.Column(image_row) 

@pn.depends(prod)    
def get_prod(prod):
    if prod == 'spread': return pn.Row(pn.Row(var_spread, width=300), fig_spread)
    if prod == 'spaguetti': return pn.Row(pn.Column(var_spaguetti, reg_spaguetti, width=300), fig_spaguetti)
    if prod == 'cluster': return pn.Row(pn.Row(var_cluster, width=300), fig_cluster)
    if prod == 'probability': return pn.Row(fig_probability)
    if prod == 'probagr': return pn.Row(pn.Row(var_probagr, width=300), fig_probagr)
    if prod == 'perturbations': return pn.Row(pn.Row(var_perturbations, width=300), fig_perturbations)
    if prod == 'chievol': return pn.Row(fig_chievol)
      
pn.template.FastListTemplate(
    site="oensMB09", title="Produtos",
    main=[pn.Row(date, prod), get_prod], 
    theme_toggle=False,
#).show();    
).servable();


# In[ ]:






await write_doc()
  `

  try {
    const [docs_json, render_items, root_ids] = await self.pyodide.runPythonAsync(code)
    self.postMessage({
      type: 'render',
      docs_json: docs_json,
      render_items: render_items,
      root_ids: root_ids
    })
  } catch(e) {
    const traceback = `${e}`
    const tblines = traceback.split('\n')
    self.postMessage({
      type: 'status',
      msg: tblines[tblines.length-2]
    });
    throw e
  }
}

self.onmessage = async (event) => {
  const msg = event.data
  if (msg.type === 'rendered') {
    self.pyodide.runPythonAsync(`
    from panel.io.state import state
    from panel.io.pyodide import _link_docs_worker

    _link_docs_worker(state.curdoc, sendPatch, setter='js')
    `)
  } else if (msg.type === 'patch') {
    self.pyodide.globals.set('patch', msg.patch)
    self.pyodide.runPythonAsync(`
    state.curdoc.apply_json_patch(patch.to_py(), setter='js')
    `)
    self.postMessage({type: 'idle'})
  } else if (msg.type === 'location') {
    self.pyodide.globals.set('location', msg.location)
    self.pyodide.runPythonAsync(`
    import json
    from panel.io.state import state
    from panel.util import edit_readonly
    if state.location:
        loc_data = json.loads(location)
        with edit_readonly(state.location):
            state.location.param.update({
                k: v for k, v in loc_data.items() if k in state.location.param
            })
    `)
  }
}

startApplication()
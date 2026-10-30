from pathlib import Path
import json,hashlib
import pandas as pd
from PIL import Image
from pypdf import PdfReader

root=Path(__file__).resolve().parents[1]
results={}
for kind,source,value,x in [('food','food_population_2050.csv','population','food'),('budget','budget_frequency_2050.csv','frequency','income')]:
    folder=root/'output'/f'{kind}_pop'
    d=pd.read_csv(folder/source)
    countries=sorted(d.R.unique())
    assert (d.Y==2050).all() and (d.seg!='ALL').all()
    assert (d[value]>0).all() and (d[x]>0).all()
    assert d.groupby('R').Ref.nunique().eq(3).all()
    assert not d.duplicated(['R','Ref','Y','seg']).any()
    if kind=='budget':
        assert ((d.income-d.BudgetInitial*(1+d.EV)).abs()/d.income).max()<1e-12
        assert not d[(d.Ref!='SSP2_BaU_NoCC_No') & d.ev_absent].shape[0]
    else:
        assert (d.MDER>0).all()
    for country in countries:
        path=folder/f'{country}_{kind}_pop_2050.png'
        with Image.open(path) as im:
            assert im.size==(1950,1575)
            im.verify()
    pdf=folder/f'{kind}_pop_2050_all_countries.pdf'
    reader=PdfReader(pdf)
    assert len(reader.pages)==len(countries)
    for country,page in zip(countries,reader.pages):
        assert country in page.extract_text()
    manifest=pd.DataFrame({'country':countries,'png':[f'{c}_{kind}_pop_2050.png' for c in countries]})
    manifest.to_csv(folder/'country_index.csv',index=False)
    results[kind]={'countries':len(countries),'scenario_count':3,'segment_records':len(d),'pdf_pages':len(reader.pages),'pngs_verified':len(countries),'input_csv':source}
q=pd.read_csv(root/'output/food_pop/plot_range_summary_both.csv')
assert q.outside_percent.max()<=.1000001
results['largest_linear_range_omitted_percent']=q.outside_percent.max()
results['inputs']={p.name:{'bytes':p.stat().st_size,'sha256':hashlib.file_digest(p.open('rb'),'sha256').hexdigest()} for p in (root/'data/FDM/最新版').glob('*.gdx')}
for kind in ['food','budget']:
    (root/'output'/f'{kind}_pop'/'validation_2050.json').write_text(json.dumps(results,indent=2),encoding='utf-8')
print(json.dumps(results,indent=2))

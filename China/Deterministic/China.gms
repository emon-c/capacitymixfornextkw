
Sets
g /Coal, Solar, Wind/

RE(g) /Solar,Wind/

t /1*24/

;

Parameters GenData(g,*), Profile(t,*)


$onecho > gdxxrwCM.in
par=GenData         rdim=1 cdim=1 rng=GenData!A2:Z70
par=Profile         rdim=1 cdim=1 rng=Profile!A2:D400
$offecho

$call gdxxrw "China.xlsx" @gdxxrwCM.in MaxDupeErrors=1000
$gdxIn "China.gdx"
$load GenData Profile
$gdxIn

display gendata, profile;


Variables

Gen(g,t)
Cap(g)
StorCap
Storage(t)
StorInj(t)
StorGen(t)

Unmet(t)
Surplus(t)

Cost

;

positive variables cap, gen, storcap, storinj, storgen, unmet, surplus;

Storage.fx("1")=0.5;
Storage.fx("24")=0.5;

StorInj.fx("1")=0;

*StorCap.fx = 0;

Gen.fx("Coal",t) = 0;
*Gen.fx(RE,t) = 0;

*Gen.fx("Wind",t) = 0;

Equations

Demand(t)
StorBal(t)
StorLim(t)
StorGenCap(t)
CoalRamp1(t)
CoalRamp2(t)
CoalMinGen(t)
Profilecon(RE,t)
CapCon(g,t)
CapCon1(g)
Obj

;

Demand(t).. Sum(g,Gen(g,t)) + StorGen(t)*0.9 + Unmet(t) - Surplus(t) =e= Profile(t,"Load")+StorInj(t)  ;


StorBal(t)$(ord(t)>1).. Storage(t) =e= Storage(t-1) + StorInj(t) - StorGen(t);


CoalRamp1(t)$(ord(t)>1).. Gen("Coal",t) =l= Gen("Coal",t-1)*1.5;

CoalRamp2(t)$(ord(t)>1).. Gen("Coal",t) =g= Gen("Coal",t-1)*0.5;

CoalMinGen(t).. Gen("Coal",t) =g= Cap("Coal")*0.4;

StorLim(t).. Storage(t) =l= StorCap;

StorGenCap(t).. StorGen(t) =l= Storage(t);

ProfileCon(RE,t).. Gen(RE,t) =e= Cap(RE)*Profile(t,RE);

CapCon(g,t).. Gen(g,t) =l= Cap(g);

CapCon1(g).. Sum(t, Gen(g,t)) =l= Cap(g)*Card(t)*0.9;

Obj.. Cost =e= Sum((t,g), Gen(g,t)*GenData(g,"opex")*365)
             + Sum(g,Cap(g)*GenData(g,"AnnCapex"))
             + Sum(g,Cap(g)*GenData(g,"FOM"))
             + StorCap*27 +StorCap*7
             + Sum(t, Unmet(t)*1 + Surplus(t)*0.05)*365
;

Model China /all/ ;

Solve China us LP min cost;

display cap.l, storcap.l, gen.l, storinj.l, storgen.l, unmet.l, surplus.l

Parameter TotalCapex;

TotalCapex =               (+ Sum(g,Cap.l(g)*GenData(g,"AnnCapex")*1000)
             + StorCap.l*27000)/1000;

display totalcapex;

Parameter Summary(*,*);

Option decimals=0;

Summary ("Coal","Cap kW") = Cap.l("Coal") ;
Summary ("Solar","Cap kW") = Cap.l("Solar") ;
Summary ("Wind","Cap kW") = Cap.l("Wind") ;
Summary ("Storage","Cap kWh") = StorCap.l ;

Summary ("Coal","Cap. ex") = Cap.l("Coal")*GenData("Coal","Capex") ;
Summary ("Solar","Cap. ex") = Cap.l("Solar")*GenData("Solar","Capex") ;
Summary ("Wind","Cap. ex") = Cap.l("Wind")*GenData("Wind","Capex") ;
Summary ("Storage","Cap. ex") = StorCap.l*180 ;
Summary ("Total","Cap. ex") = Summary ("Coal","Cap. ex")+ Summary ("Solar","Cap. ex")+Summary ("Wind","Cap. ex")+Summary ("Storage","Cap. ex");

Summary ("Coal","Ann. Capex") = Cap.l("Coal")*GenData("Coal","AnnCapex") ;
Summary ("Solar","Ann. Capex") = Cap.l("Solar")*GenData("Solar","AnnCapex") ;
Summary ("Wind","Ann. Capex") = Cap.l("Wind")*GenData("Wind","AnnCapex") ;
Summary ("Storage","Ann. Capex") = StorCap.l*27 ;
Summary ("Total","Ann. Capex") = Summary ("Coal","Ann. Capex")+ Summary ("Solar","Ann. Capex")+Summary ("Wind","Ann. Capex")+Summary ("Storage","Ann. Capex");

Summary ("Coal","Fixed O&M") = Cap.l("Coal")*GenData("Coal","FOM") ;
Summary ("Solar","Fixed O&M") = Cap.l("Solar")*GenData("Solar","FOM") ;
Summary ("Wind","Fixed O&M") = Cap.l("Wind")*GenData("Wind","FOM") ;
Summary ("Storage","Fixed O&M") = StorCap.l*7 ;
Summary ("Total","Fixed O&M") = Summary ("Coal","Fixed O&M")+ Summary ("Solar","Fixed O&M")+Summary ("Wind","Fixed O&M")+Summary ("Storage","Fixed O&M");

Summary ("Coal","Opex") = Sum((t), Gen.l("Coal",t)*GenData("Coal","opex")*365) ;
Summary ("Total","Opex") = Summary ("Coal","Opex");

Summary ("Total","Ann. Cost") = Summary ("Coal","Opex") +Summary ("Total","Fixed O&M")+ Summary ("Total","Ann. Capex");


display summary;

execute_unload 'RESULTS.gdx', summary, gen.l, storgen.l, storage.l;

* Write the data from the GDX file to the Excel file using gdxxrw
execute 'gdxxrw RESULTS.gdx par=Summary rng=Summary!A5 var=Gen rng=Gen!A5  var=StorGen rng=Storage_Discharge!A5 var=Storage rng=Storage_Level!A5';

$inlinecom { }

Scalars
CoalRamp /0.5/
GasRamp /0.5/

Sets
g /Coal, Solar, Wind, Gas/

RE(g) /Solar,Wind/

t /1*24/

s RE scenarios /Avg, Low, High/ {I am just going to assume High=1.2*Avg and Low=0.8*Avg}

;

Parameters GenData(g,*), Profile(t,*), REUncert(g,s,*)


$onecho > gdxxrwCM.in
par=GenData         rdim=1 cdim=1 rng=GenData!A2:Z70
par=Profile         rdim=1 cdim=1 rng=Profile!A2:D400
par=REUncert        rdim=2 cdim=1 rng=REUncert!A3:D100
$offecho

$call gdxxrw "Australia.xlsx" @gdxxrwCM.in MaxDupeErrors=1000
$gdxIn "Australia.gdx"
$load GenData Profile REUncert
$gdxIn

display gendata, profile, reuncert;



Variables

Gen(g,s,t)
Cap(g)
StorCap
Storage(s,t)
StorInj(s,t)
StorGen(s,t)

Unmet(t)
Surplus(t)

Cost

;

positive variables cap, gen, storcap, storinj, storgen, unmet, surplus;

Storage.fx(s,"1")=1;
Storage.fx(s,"24")=1;

StorInj.fx(s,"1")=0;

*StorCap.fx = 0;

*Gen.fx("Coal",s,t) = 0;
*Gen.fx(RE,s,t) = 0;
*Cap.fx("Solar") = 1;

*Gen.fx("Wind",s,t) = 0;

Gen.fx(g,s,t)$(REUncert(g,s,"Prob")=0) = 0;


Equations

Demand(s,t)
StorBal(s,t)
StorLim(s,t)
StorGenCap(s,t)
CoalRamp1(s,t)
CoalRamp2(s,t)
CoalMinGen(s,t)
GasRamp1(s,t)
GasRamp2(s,t)
GasMinGen(s,t)
Profilecon(RE,s,t)
CapCon(g,s,t)
CapCon1(g,s)
Obj

;

Demand(s,t)$(Sum(g,REUncert(g,s,"Prob"))>0).. Sum(g,Gen(g,s,t)) + StorGen(s,t)*0.9 + Unmet(t) - Surplus(t) =e=
Profile(t,"Load")+StorInj(s,t)  ;


StorBal(s,t)$(ord(t)>1).. Storage(s,t) =e= Storage(s,t-1) + StorInj(s,t) - StorGen(s,t);



CoalRamp1(s,t).. Gen("Coal",s,t) =l= Gen("Coal",s,t)*(1 + CoalRamp);

CoalRamp2(s,t).. Gen("Coal",s,t) =g= Gen("Coal",s,t)*(1 - CoalRamp);

CoalMinGen(s,t).. Gen("Coal",s,t) =g= Cap("Coal")*0.4;



GasRamp1(s,t).. Gen("Gas",s,t) =l= Gen("Gas",s,t)*(1 + GasRamp);

GasRamp2(s,t).. Gen("Gas",s,t) =g= Gen("Gas",s,t)*(1 - GasRamp);

GasMinGen(s,t).. Gen("Gas",s,t) =g= Cap("Gas")*0.4;


StorLim(s,t).. Storage(s,t) =l= StorCap;

StorGenCap(s,t).. StorGen(s,t) =l= Storage(s,t);

ProfileCon(RE,s,t).. Gen(RE,s,t) =l= Cap(RE)*Profile(t,RE)*REUncert(RE,s,"Scale");

CapCon(g,s,t).. Gen(g,s,t) =l= Cap(g);

CapCon1(g,s).. Sum(t, Gen(g,s,t)) =l= Cap(g)*Card(t)*0.9;

Obj.. Cost =e= Sum((t,s,g), Gen(g,s,t)*GenData(g,"opex")*365*REUncert(g,s,"Prob"))
             + Sum(g,Cap(g)*GenData(g,"AnnCapex"))
             + Sum(g,Cap(g)*GenData(g,"FOM"))
             + StorCap*27 +StorCap*7
             + Sum((t), (Unmet(t)*1 + Surplus(t)*0.5))*365
;

Model Australia /all/ ;

Solve Australia us LP min cost;

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

Summary ("Coal","Opex") = Sum((s,t), Gen.l("Coal",s,t)*GenData("Coal","opex")*365*REUncert("Coal",s,"Prob")) ;
Summary ("Total","Opex") = Summary ("Coal","Opex");

Summary ("Total","Ann. Cost") = Summary ("Coal","Opex") +Summary ("Total","Fixed O&M")+ Summary ("Total","Ann. Capex");


display summary;

execute_unload 'RESULTS.gdx', summary, gen.l, storgen.l, storage.l;

* Write the data from the GDX file to the Excel file using gdxxrw
execute 'gdxxrw RESULTS.gdx par=Summary rng=Summary!A5 var=Gen rng=Gen!A5  var=StorGen rng=Storage_Discharge!A5 var=Storage rng=Storage_Level!A5';

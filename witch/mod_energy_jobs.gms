*------------------------------------------------------------------------------
* Module Name: Module Template
* Explain in brief what your module is for
* -----------------------------------------------------------------------------
* Cite your important sources (paper,...)
* -----------------------------------------------------------------------------
* Short changelog with date/name
*------------------------------------------------------------------------------

$ifthen %phase%=='conf'

$setglobal global_pool

*------------------------------------------------------------------------------
$elseif %phase%=='sets'

* In the phase SETS you should declare all your sets, or add to the existing
* sets the element that you need.

set jobs_category / oem, construction, manufacturing, extraction, refinery /;
set jobs_fuel /biogas,biomass,coal,gas,geothermal,hydro,nuclear,oil,solar_csp,solar_pv,trbiofuel,wind_on,wind_off /;

set map_jobs_fuel_fuel(jobs_fuel, fuel)/
coal.coal
gas.gas
oil.oil
trbiofuel.trbiofuel
nuclear.uranium
/;

set map_jobs_fuel_jreal(jobs_fuel, jreal)/
coal.elpc_new
coal.elpc_old
coal.elpc_late
coal.elpc_ccs
coal.elpc_oxy
coal.elcigcc
nuclear.elnuclear_old
nuclear.elnuclear_new
oil.eloil_new
oil.eloil_old
gas.elgastr_new
gas.elgastr_old
gas.elgasccs
hydro.elhydro_new
hydro.elhydro_old
biomass.elpb_new
biomass.elpb_old
biomass.elbigcc
wind_on.elwindon
wind_off.elwindoff
solar_pv.elpv
solar_csp.elcsp
/;





*------------------------------------------------------------------------------
$elseif %phase%=='include_data'

* In the phase INCLUDE_DATA you should declare and include all exogenous parameters.
* You can also modify the data loaded in data.gms
* Best practice : - create a .gdx containing those and to loading it
*                 - this is the only phase where we should have numbers...

Parameter energy_jobs_intensity(jobs_category, jobs_fuel, t, n);
Parameter global_production_shares(jobs_category, jobs_fuel, t, n);

$gdxin '%datapath%data_mod_energy_jobs%nrg_jobs_bnd%.gdx'
$load energy_jobs_intensity
$load global_production_shares
$gdxin

*Get conventional / non-conventional shares for Q_OUT (extraction) for oil and coal (gas not availabe in WEO)
*$gdxin %datapath%data_validation
*parameter q_out_type_valid_weo(*,t,n);
*$load q_out_type_valid_weo
*$gdxin

*------------------------------------------------------------------------------
$elseif %phase%=='compute_data'

* In the phase COMPUTE_DATA you should declare and compute all the parameters
* that depend on the data loaded in the previous phase.

* Start by putting consant shares and job intensities over time
energy_jobs_intensity(jobs_category, jobs_fuel, t, n) = valuein(2005, energy_jobs_intensity(jobs_category, jobs_fuel, tt, n));
global_production_shares(jobs_category, jobs_fuel, t, n) = valuein(2005, global_production_shares(jobs_category, jobs_fuel, tt, n));

* for NA values take corresponding oecd/non oecd simple mean
energy_jobs_intensity(jobs_category, jobs_fuel, t, n)$(oecd(n) and not energy_jobs_intensity(jobs_category, jobs_fuel, t, n)) = div0( sum(nn$(oecd(nn) and energy_jobs_intensity(jobs_category, jobs_fuel, t, nn)), energy_jobs_intensity(jobs_category, jobs_fuel, t, nn)) , sum(nn$(oecd(nn) and energy_jobs_intensity(jobs_category, jobs_fuel, t, nn)), 1) );
energy_jobs_intensity(jobs_category, jobs_fuel, t, n)$(not oecd(n) and not energy_jobs_intensity(jobs_category, jobs_fuel, t, n)) = div0( sum(nn$(not oecd(nn) and energy_jobs_intensity(jobs_category, jobs_fuel, t, nn)), energy_jobs_intensity(jobs_category, jobs_fuel, t, nn)) , sum(nn$(not oecd(nn) and energy_jobs_intensity(jobs_category, jobs_fuel, t, nn)), 1) );


* set not used data to zero
energy_jobs_intensity('extraction', jobs_fuel, t, n)$(not (sameas(jobs_fuel, "oil") or sameas(jobs_fuel, "gas") or sameas(jobs_fuel, "coal") or sameas(jobs_fuel, "nuclear"))) = 0; #extraction only for gas, coal, oil, and nuclear
energy_jobs_intensity('manufacturing', jobs_fuel, t, n)$(sameas(jobs_fuel, "trbiofuel") or sameas(jobs_fuel, "biogas")) = 0; #manufacturing
energy_jobs_intensity('construction', jobs_fuel, t, n)$(sameas(jobs_fuel, "trbiofuel") or sameas(jobs_fuel, "biogas"))= 0; #construction
energy_jobs_intensity('construction', 'oil', t, n) = energy_jobs_intensity('construction', 'gas', t, n) = 0; #construction of oil fired power plants equal to gas
energy_jobs_intensity('oem', 'oil', t, n) = energy_jobs_intensity('oem', 'gas', t, n) = 0; #oem of oil fired power plants equal to gas
energy_jobs_intensity('oem', jobs_fuel, t, n)$(sameas(jobs_fuel, "trbiofuel") or sameas(jobs_fuel, "biogas")) = 0; #oem
energy_jobs_intensity('refinery', jobs_fuel, t, n)$(not (sameas(jobs_fuel, "oil") or sameas(jobs_fuel, "trbiofuel"))) = 0; #refinery only for oil (refinery) and trbiofuel (production & refinery)

* Technological progress implementation
* convergence of non-oecd to oecd standard by 2100 exponentially, only in case oecd is lower
energy_jobs_intensity(jobs_category, jobs_fuel, t, n)$(year(t) gt 2015 and year(t) le 2100 and not oecd(n) and energy_jobs_intensity(jobs_category, jobs_fuel, t, n) ne 0) = min(
energy_jobs_intensity(jobs_category, jobs_fuel, '3', n) * exp( (1/85) * log((sum(nn$oecd(nn),energy_jobs_intensity(jobs_category, jobs_fuel, '3', nn))/sum(nn$oecd(nn),1))/energy_jobs_intensity(jobs_category, jobs_fuel, '3', n)) * (year(t) - 2015)),
    energy_jobs_intensity(jobs_category, jobs_fuel, '3', n) );
energy_jobs_intensity(jobs_category, jobs_fuel, t, n)$(year(t) gt 2100) = valuein(2100, energy_jobs_intensity(jobs_category, jobs_fuel, tt, n));
*    ( ((2100 - year(t))/85)*energy_jobs_intensity(jobs_category, jobs_fuel, '3', n) + (sum(nn$oecd(nn),energy_jobs_intensity(jobs_category, jobs_fuel, '20', nn))/sum(nn$oecd(nn),1))*((year(t)-2015)/85) ),



*------------------------------------------------------------------------------
$elseif %phase%=='vars'

* In the phase VARS, you can declare new variables for your module.
* Remember that by modifying sets, you already have some variables for free.
* Best practice : - for variables that are fixed for the first time period =>
*                                                                       MY_VARIABLE.fx(tfirst,n)=my_variable0(n);
*                 - load a good initial point from a gdx

Variable ENERGY_JOBS(jobs_category, jobs_fuel, t, n);
Variable ENERGY_JOBS_GLOBAL_POOL(jobs_category, jobs_fuel, t);
*------------------------------------------------------------------------------
$elseif %phase%=='policy'

* In the phase POLICY, you should specify your policy
* Best practice : This phase should be used mainly when you write a project file
*                 and you declare the policy according to your scenarios


*------------------------------------------------------------------------------
$elseif %phase%=='eql'

* List of equations
* One per line.

*------------------------------------------------------------------------------
$elseif %phase%=='eqs'


* In the phase EQS, you can include new equations to the model.
* The equations are always included.
* Best practice : - condition your equation to be able to do a run with tfix(t)




*------------------------------------------------------------------------------
$elseif %phase%=='fix_variables'

* This phase is done after the phase POLICY.
* You should fix all your new variables.
* Remember, quantities are fixed up to the last tfix, while investment only
* up to tfix-1
* Best practices :
* - Use the provided macro in policy_fix
* - Your module should be able to do a tfix run

*------------------------------------------------------------------------------
$elseif %phase%=='before_nashloop'

* The phase BEFORE_NASHLOOP is ran just before the loop solving an equilibrium

*------------------------------------------------------------------------------
$elseif %phase%=='before_solve'

* In the phase BEFORE_SOLVE, you can update parameters (fixed
* variables, ...) inside the nash loop and right before solving the
* model. This is typically done for externalities, spillovers, ...
* Best practice: record the variable that you update across iterations.
* Remember that you are inside the nash loop, so you cannot declare
* parameters, ...
* This phase is recalled one last time after the last nash iteration.



*------------------------------------------------------------------------------
$elseif %phase%=='after_solve'
* In the phase AFTER_NASHLOOP, you compute parameters needed for the report,
* once the job is ready.
* Best practice :
*      - you should not declare parameters as this phase might be inside a loop

* compute job values based on intensities and energy quantities from the model
* Manufacturing of everything except renewables is assumed in the host country (no global pool)
ENERGY_JOBS.l('manufacturing', jobs_fuel, t, n)$(not sameas(jobs_fuel, 'solar_pv') and not sameas(jobs_fuel, 'solar_csp') and not sameas(jobs_fuel, 'wind_on') and not sameas(jobs_fuel, 'wind_off')) = sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) );
ENERGY_JOBS.l('oem', jobs_fuel, t, n)$(not sameas(jobs_fuel, 'oil')) = sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('oem', jobs_fuel, t, n) * K_EN.l(jreal, t, n));
ENERGY_JOBS.l('construction', jobs_fuel, t, n) = sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('construction', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n));
ENERGY_JOBS.l('extraction', jobs_fuel, t, n)$(not sameas(jobs_fuel, 'nuclear')) = sum(fuel$map_jobs_fuel_fuel(jobs_fuel, fuel), energy_jobs_intensity('extraction', jobs_fuel, t, n) * sum(f$sameas(f,fuel), Q_OUT.l(f, t, n)) );
ENERGY_JOBS.l('refinery', jobs_fuel, t, n) = sum(fuel$map_jobs_fuel_fuel(jobs_fuel, fuel), energy_jobs_intensity('refinery', jobs_fuel, t, n) * Q_FUEL.l(fuel, t, n) );
*Nuclear extraction is based on generation of nuclear using constant extraction shares
ENERGY_JOBS.l('extraction', jobs_fuel, t, n)$(sameas(jobs_fuel, 'nuclear'))  = sum(fuel$map_jobs_fuel_fuel(jobs_fuel, fuel), energy_jobs_intensity('extraction', jobs_fuel, t, n) * (global_production_shares('extraction', jobs_fuel, t, n)/100) * sum(nn, sum(f$sameas(f,fuel), Q_FUEL.l(f, t, nn)) ) );

$ifthen.gp set global_pool
*global pool for manufacturing RENEWABLES
ENERGY_JOBS.l('manufacturing', 'solar_pv', t, n) = min(energy_jobs_intensity('manufacturing', 'solar_pv', '3', n) * (global_production_shares('manufacturing', 'solar_pv', '3', n)/100) * sum(nn, I_EN.l('elpv', '3', nn) / MCOST_INV.l('elpv','3',nn)), energy_jobs_intensity('manufacturing', 'solar_pv', t, n) * (global_production_shares('manufacturing', 'solar_pv', t, n)/100) * sum(nn, I_EN.l('elpv', t, nn) / MCOST_INV.l('elpv',t,nn)));
ENERGY_JOBS_GLOBAL_POOL.l('manufacturing', 'solar_pv', t) = sum(n, ( energy_jobs_intensity('manufacturing', 'solar_pv', t, n) * (global_production_shares('manufacturing', 'solar_pv', t, n)/100) * sum(nn, I_EN.l('elpv', t, nn) / MCOST_INV.l('elpv',t,nn)) ) - ( min(energy_jobs_intensity('manufacturing', 'solar_pv', '3', n) * (global_production_shares('manufacturing', 'solar_pv', '3', n)/100) * sum(nn, I_EN.l('elpv', '3', nn) / MCOST_INV.l('elpv','3',nn)), energy_jobs_intensity('manufacturing', 'solar_pv', t, n) * (global_production_shares('manufacturing', 'solar_pv', t, n)/100) * sum(nn, I_EN.l('elpv', t, nn) / MCOST_INV.l('elpv',t,nn))) ) );
*other renewables than Solar PV based on consumption in 2015, then rest to global pool
*ENERGY_JOBS.l('manufacturing', jobs_fuel, t, n)$(sameas(jobs_fuel, 'solar_csp') or sameas(jobs_fuel, 'wind_on') or sameas(jobs_fuel, 'wind_off')) = min(sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, '3', n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, '3', n)) / MCOST_INV.l(jreal,'3',n) ), sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) ) );
*ENERGY_JOBS_GLOBAL_POOL.l('manufacturing',jobs_fuel, t)$(sameas(jobs_fuel, 'solar_csp') or sameas(jobs_fuel, 'wind_on') or sameas(jobs_fuel, 'wind_off')) = sum(n, sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) ) - ( min(sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, '3', n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, '3', n)) / MCOST_INV.l(jreal,'3',n) ), sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) ) ) ));
*For now, All other renewables have the global pool already for today and of all installed capacity as we don't have data on where they are manufactured
ENERGY_JOBS.l('manufacturing', jobs_fuel, t, n)$(sameas(jobs_fuel, 'solar_csp') or sameas(jobs_fuel, 'wind_on') or sameas(jobs_fuel, 'wind_off')) = 0;
ENERGY_JOBS_GLOBAL_POOL.l('manufacturing',jobs_fuel, t)$(sameas(jobs_fuel, 'solar_csp') or sameas(jobs_fuel, 'wind_on') or sameas(jobs_fuel, 'wind_off')) = sum(n, sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) ) );
$else.gp
*All manufacturing based on installment in each country, except EL_PV which is based on current manufacturing shares kept for the future
ENERGY_JOBS.l('manufacturing', 'solar_pv', t, n) = energy_jobs_intensity('manufacturing', 'solar_pv', t, n) * (global_production_shares('manufacturing', 'solar_pv', t, n)/100) * sum(nn, I_EN.l('elpv', t, nn) / MCOST_INV.l('elpv',t,nn));
ENERGY_JOBS.l('manufacturing', jobs_fuel, t, n)$(sameas(jobs_fuel, 'solar_csp') or sameas(jobs_fuel, 'wind_on') or sameas(jobs_fuel, 'wind_off')) = sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) );
$endif.gp
*Manufacturing of fossil fuel based power plants and nuclear are just within the country of installation
ENERGY_JOBS.l('manufacturing', jobs_fuel, t, n)$(sameas(jobs_fuel, 'coal') or sameas(jobs_fuel, 'gas') or sameas(jobs_fuel, 'oil') and sameas(jobs_fuel, 'nuclear')) = sum(jreal$map_jobs_fuel_jreal(jobs_fuel, jreal), energy_jobs_intensity('manufacturing', jobs_fuel, t, n) * sum(jinv$sameas(jinv,jreal),I_EN.l(jinv, t, n)) / MCOST_INV.l(jreal,t,n) );


*------------------------------------------------------------------------------
$elseif %phase%=='summary_report'

* parameter necessary for database



*------------------------------------------------------------------------------
$elseif %phase%=='gdx_items'
* List the items to be kept in the final gdx
energy_jobs_intensity
global_production_shares
ENERGY_JOBS
ENERGY_JOBS_GLOBAL_POOL

$endif

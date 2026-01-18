/**
 * COP30 Organization Trees Enhancement
 * Adds accurate priorities and contributor networks for all 143 organizations
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/demo/orgs.json', 'utf8'));

// Helper to add contributors to nodes
function addContributors(tree, nodeId, contributors) {
  function traverse(node) {
    if (node.id === nodeId && node.contributors) {
      node.contributors = contributors;
      return true;
    }
    if (node.children) {
      for (const child of node.children) {
        if (traverse(child)) return true;
      }
    }
    return false;
  }
  traverse(tree.tree);
}

// Helper to create contributor
const c = (id, points) => ({ id, points });

// ═══════════════════════════════════════════════════════════════════
// ENHANCE INTERNATIONAL ORGANIZATIONS
// ═══════════════════════════════════════════════════════════════════

// ILO - International Labour Organization
if (trees.ilo) {
  addContributors(trees.ilo, 'ilo_root_just_transition', [
    c('org_demo_undp', 35),
    c('org_demo_unep', 30),
    c('org_demo_worldbank', 20)
  ]);
}

// IAEA - International Atomic Energy Agency  
if (trees.iaea) {
  addContributors(trees.iaea, 'iaea_root_nuclear_energy', [
    c('org_demo_iaea', 45),
    c('org_demo_wna', 30),
    c('org_demo_ens', 20)
  ]);
}

// IRENA - International Renewable Energy Agency
if (trees.irena) {
  addContributors(trees.irena, 'irena_root_deployment', [
    c('org_demo_isa', 40),
    c('org_demo_wwea', 35),
    c('org_demo_worldbank', 25),
    c('org_demo_greenclimatefund', 20)
  ]);
  addContributors(trees.irena, 'irena_root_transition', [
    c('org_demo_unep', 35),
    c('org_demo_unfccc', 30),
    c('org_demo_threefiveozero', 20)
  ]);
}

// ISO - International Organization for Standardization
if (trees.iso) {
  addContributors(trees.iso, 'iso_root_climate_standards', [
    c('org_demo_ipcc', 35),
    c('org_demo_unfccc', 30),
    c('org_demo_wemeanbus', 20)
  ]);
  addContributors(trees.iso, 'iso_root_energy_eff', [
    c('org_demo_unhabitat', 30),
    c('org_demo_c40cities', 25),
    c('org_demo_iclei', 20)
  ]);
}

// IOM - International Organization for Migration
if (trees.iom) {
  addContributors(trees.iom, 'iom_root_climate_migration', [
    c('org_demo_unhcr', 40),
    c('org_demo_undp', 35),
    c('org_demo_aosis', 25),
    c('org_demo_ldc_group', 20)
  ]);
  addContributors(trees.iom, 'iom_root_protection', [
    c('org_demo_unicef', 40),
    c('org_demo_who', 30),
    c('org_demo_wfp', 25)
  ]);
}

// WMO - World Meteorological Organization
if (trees.wmo) {
  addContributors(trees.wmo, 'wmo_root_monitoring', [
    c('org_demo_ipcc', 40),
    c('org_demo_unep', 30),
    c('org_demo_unfccc', 25)
  ]);
  addContributors(trees.wmo, 'wmo_root_prediction', [
    c('org_demo_fao', 35),
    c('org_demo_wfp', 30),
    c('org_demo_cdri', 25)
  ]);
}

// UNFCCC - United Nations Climate Change
if (trees.unfccc) {
  addContributors(trees.unfccc, 'unfccc_root_paris', [
    c('org_demo_ipcc', 45),
    c('org_demo_unep', 40),
    c('org_demo_undp', 35),
    c('org_demo_europeanunion', 30),
    c('org_demo_ndc_partnership', 25)
  ]);
  addContributors(trees.unfccc, 'unfccc_root_finance', [
    c('org_demo_greenclimatefund', 50),
    c('org_demo_worldbank', 40),
    c('org_demo_climate_funds', 30)
  ]);
  addContributors(trees.unfccc, 'unfccc_root_loss_damage', [
    c('org_demo_aosis', 45),
    c('org_demo_ldc_group', 40),
    c('org_demo_africanunion', 30)
  ]);
}

// NEP - Negative Emissions Platform
if (trees.nep) {
  addContributors(trees.nep, 'nep_root_cdr', [
    c('org_demo_ipcc', 40),
    c('org_demo_gatesfoundation', 35),
    c('org_demo_bezosearthfund', 25)
  ]);
  addContributors(trees.nep, 'nep_root_nature', [
    c('org_demo_wwf', 40),
    c('org_demo_natureconservancy', 35),
    c('org_demo_fao', 30),
    c('org_demo_cfrn', 25)
  ]);
}

// ICLEI - Local Governments for Sustainability
if (trees.iclei) {
  addContributors(trees.iclei, 'iclei_root_urban', [
    c('org_demo_c40cities', 45),
    c('org_demo_unhabitat', 40),
    c('org_demo_worldbank', 30),
    c('org_demo_bloombergphilanthropies', 25)
  ]);
  addContributors(trees.iclei, 'iclei_root_nature_urban', [
    c('org_demo_natureconservancy', 40),
    c('org_demo_wwf', 30),
    c('org_demo_unep', 25)
  ]);
}

// CDRI - Coalition for Disaster Resilient Infrastructure
if (trees.cdri) {
  addContributors(trees.cdri, 'cdri_root_resilient_infra', [
    c('org_demo_worldbank', 45),
    c('org_demo_asiandevbank', 35),
    c('org_demo_africandevbank', 30),
    c('org_demo_iadb', 25)
  ]);
  addContributors(trees.cdri, 'cdri_root_disaster', [
    c('org_demo_wmo', 40),
    c('org_demo_undp', 35),
    c('org_demo_redcross', 30)
  ]);
}

// ICC Chamber - International Chamber of Commerce
if (trees.icc_chamber) {
  addContributors(trees.icc_chamber, 'icc_chamber_root_business', [
    c('org_demo_wemeanbus', 45),
    c('org_demo_iso', 35),
    c('org_demo_unfccc', 25)
  ]);
  addContributors(trees.icc_chamber, 'icc_chamber_root_green_finance', [
    c('org_demo_greenclimatefund', 40),
    c('org_demo_worldbank', 35),
    c('org_demo_climate_funds', 25)
  ]);
}

// SIWI - Stockholm International Water Institute
if (trees.siwi) {
  addContributors(trees.siwi, 'siwi_root_water_climate', [
    c('org_demo_wmo', 40),
    c('org_demo_fao', 35),
    c('org_demo_unep', 30),
    c('org_demo_undp', 25)
  ]);
  addContributors(trees.siwi, 'siwi_root_governance', [
    c('org_demo_unep', 40),
    c('org_demo_undp', 30)
  ]);
}

// ISA - International Solar Alliance
if (trees.isa) {
  addContributors(trees.isa, 'isa_root_solar', [
    c('org_demo_irena', 50),
    c('org_demo_india', 40),
    c('org_demo_morocco', 30),
    c('org_demo_chile', 25)
  ]);
  addContributors(trees.isa, 'isa_root_finance', [
    c('org_demo_greenclimatefund', 45),
    c('org_demo_worldbank', 40),
    c('org_demo_asiandevbank', 30)
  ]);
}

// INTOSAI - Supreme Audit Institutions
if (trees.intosai) {
  addContributors(trees.intosai, 'intosai_root_audit', [
    c('org_demo_unfccc', 40),
    c('org_demo_worldbank', 35),
    c('org_demo_greenclimatefund', 30)
  ]);
}

// Ramsar - Wetlands Convention
if (trees.ramsar) {
  addContributors(trees.ramsar, 'ramsar_root_wetlands', [
    c('org_demo_unep', 45),
    c('org_demo_wwf', 40),
    c('org_demo_natureconservancy', 35),
    c('org_demo_fao', 25)
  ]);
  addContributors(trees.ramsar, 'ramsar_root_climate', [
    c('org_demo_ipcc', 40),
    c('org_demo_unep', 35)
  ]);
}

// NDC Partnership
if (trees.ndc_partnership) {
  addContributors(trees.ndc_partnership, 'ndc_partnership_root_ndc', [
    c('org_demo_unfccc', 50),
    c('org_demo_undp', 45),
    c('org_demo_unep', 35),
    c('org_demo_worldbank', 30)
  ]);
  addContributors(trees.ndc_partnership, 'ndc_partnership_root_mobilization', [
    c('org_demo_greenclimatefund', 45),
    c('org_demo_worldbank', 40),
    c('org_demo_asiandevbank', 30)
  ]);
}

// CfRN - Coalition for Rainforest Nations
if (trees.cfrn) {
  addContributors(trees.cfrn, 'cfrn_root_redd', [
    c('org_demo_fao', 45),
    c('org_demo_unep', 40),
    c('org_demo_worldbank', 35),
    c('org_demo_brazil', 30),
    c('org_demo_indonesia', 25)
  ]);
  addContributors(trees.cfrn, 'cfrn_root_protection', [
    c('org_demo_conservationinternational', 45),
    c('org_demo_wwf', 35),
    c('org_demo_indigenous_pavilion', 30)
  ]);
}

// WWEA - World Wind Energy Association
if (trees.wwea) {
  addContributors(trees.wwea, 'wwea_root_wind', [
    c('org_demo_irena', 50),
    c('org_demo_denmark', 40),
    c('org_demo_uk', 35),
    c('org_demo_germany', 30)
  ]);
  addContributors(trees.wwea, 'wwea_root_policy', [
    c('org_demo_unfccc', 40),
    c('org_demo_irena', 35)
  ]);
}

// Climate Registry
if (trees.climate_registry) {
  addContributors(trees.climate_registry, 'climate_registry_root_measurement', [
    c('org_demo_ipcc', 45),
    c('org_demo_unfccc', 40),
    c('org_demo_iso', 30)
  ]);
  addContributors(trees.climate_registry, 'climate_registry_root_accounting', [
    c('org_demo_wemeanbus', 40),
    c('org_demo_iso', 35)
  ]);
}

// WGEO - World Green Economy Organization
if (trees.wgeo) {
  addContributors(trees.wgeo, 'wgeo_root_economy', [
    c('org_demo_undp', 45),
    c('org_demo_unep', 40),
    c('org_demo_worldbank', 35)
  ]);
  addContributors(trees.wgeo, 'wgeo_root_innovation', [
    c('org_demo_irena', 40),
    c('org_demo_gatesfoundation', 30)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCE MAJOR COUNTRIES
// ═══════════════════════════════════════════════════════════════════

// China
if (trees.china) {
  addContributors(trees.china, 'china_root_emissions', [
    c('org_demo_unfccc', 45),
    c('org_demo_unep', 40),
    c('org_demo_ipcc', 30)
  ]);
  addContributors(trees.china, 'china_root_renewable', [
    c('org_demo_irena', 50),
    c('org_demo_isa', 40),
    c('org_demo_wwea', 35)
  ]);
}

// United States (if exists as US or usa)
// India
if (trees.india) {
  addContributors(trees.india, 'india_root_solar', [
    c('org_demo_isa', 50),
    c('org_demo_irena', 45),
    c('org_demo_worldbank', 30)
  ]);
  addContributors(trees.india, 'india_root_adaptation', [
    c('org_demo_undp', 45),
    c('org_demo_greenclimatefund', 35),
    c('org_demo_asiandevbank', 30)
  ]);
}

// Germany
if (trees.germany) {
  addContributors(trees.germany, 'germany_root_energiewende', [
    c('org_demo_irena', 50),
    c('org_demo_wwea', 45),
    c('org_demo_europeanunion', 40)
  ]);
  addContributors(trees.germany, 'germany_root_finance', [
    c('org_demo_greenclimatefund', 50),
    c('org_demo_worldbank', 40),
    c('org_demo_kfw', 35)
  ]);
}

// United Kingdom
if (trees.uk) {
  addContributors(trees.uk, 'uk_root_net_zero', [
    c('org_demo_unep', 45),
    c('org_demo_unfccc', 40),
    c('org_demo_wwea', 35)
  ]);
  addContributors(trees.uk, 'uk_root_finance', [
    c('org_demo_greenclimatefund', 50),
    c('org_demo_worldbank', 40)
  ]);
}

// France
if (trees.france) {
  addContributors(trees.france, 'france_root_paris_leadership', [
    c('org_demo_unfccc', 50),
    c('org_demo_europeanunion', 45),
    c('org_demo_unep', 35)
  ]);
  addContributors(trees.france, 'france_root_finance', [
    c('org_demo_greenclimatefund', 45),
    c('org_demo_africandevbank', 40)
  ]);
}

// Brazil
if (trees.brazil) {
  addContributors(trees.brazil, 'brazil_root_amazon', [
    c('org_demo_fao', 50),
    c('org_demo_conservationinternational', 45),
    c('org_demo_wwf', 40),
    c('org_demo_ipam', 35),
    c('org_demo_cfrn', 30)
  ]);
  addContributors(trees.brazil, 'brazil_root_renewable', [
    c('org_demo_irena', 45),
    c('org_demo_iadb', 35),
    c('org_demo_unica', 30)
  ]);
}

// Australia
if (trees.australia) {
  addContributors(trees.australia, 'australia_root_renewable', [
    c('org_demo_isa', 50),
    c('org_demo_irena', 45),
    c('org_demo_wwea', 35)
  ]);
  addContributors(trees.australia, 'australia_root_pacific', [
    c('org_demo_aosis', 50),
    c('org_demo_undp', 40),
    c('org_demo_greenclimatefund', 35)
  ]);
}

// Denmark
if (trees.denmark) {
  addContributors(trees.denmark, 'denmark_root_offshore_wind', [
    c('org_demo_wwea', 50),
    c('org_demo_irena', 45),
    c('org_demo_europeanunion', 35)
  ]);
  addContributors(trees.denmark, 'denmark_root_finance', [
    c('org_demo_greenclimatefund', 50),
    c('org_demo_undp', 40)
  ]);
}

// Morocco
if (trees.morocco) {
  addContributors(trees.morocco, 'morocco_root_solar', [
    c('org_demo_isa', 50),
    c('org_demo_irena', 45),
    c('org_demo_worldbank', 35),
    c('org_demo_africandevbank', 30)
  ]);
  addContributors(trees.morocco, 'morocco_root_cop_hosting', [
    c('org_demo_unfccc', 50),
    c('org_demo_africanunion', 35)
  ]);
}

// Chile
if (trees.chile) {
  addContributors(trees.chile, 'chile_root_renewable', [
    c('org_demo_isa', 50),
    c('org_demo_irena', 45),
    c('org_demo_wwea', 35),
    c('org_demo_iadb', 30)
  ]);
}

// Indonesia
if (trees.indonesia) {
  addContributors(trees.indonesia, 'indonesia_root_deforestation', [
    c('org_demo_fao', 50),
    c('org_demo_wwf', 45),
    c('org_demo_cfrn', 40),
    c('org_demo_conservationinternational', 35)
  ]);
  addContributors(trees.indonesia, 'indonesia_root_renewable', [
    c('org_demo_irena', 45),
    c('org_demo_asiandevbank', 35)
  ]);
}

// Ethiopia
if (trees.ethiopia) {
  addContributors(trees.ethiopia, 'ethiopia_root_green_economy', [
    c('org_demo_irena', 45),
    c('org_demo_fao', 40),
    c('org_demo_undp', 35),
    c('org_demo_africandevbank', 30)
  ]);
  addContributors(trees.ethiopia, 'ethiopia_root_adaptation', [
    c('org_demo_wfp', 40),
    c('org_demo_undp', 35),
    c('org_demo_fao', 30)
  ]);
}

// South Korea
if (trees.south_korea) {
  addContributors(trees.south_korea, 'south_korea_root_green_new_deal', [
    c('org_demo_greenclimatefund', 45),
    c('org_demo_worldbank', 35),
    c('org_demo_asiandevbank', 30)
  ]);
  addContributors(trees.south_korea, 'south_korea_root_technology', [
    c('org_demo_irena', 45),
    c('org_demo_ipcc', 30)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCE DEVELOPMENT BANKS
// ═══════════════════════════════════════════════════════════════════

// AfDB - African Development Bank
if (trees.afdb) {
  addContributors(trees.afdb, 'afdb_root_finance', [
    c('org_demo_worldbank', 45),
    c('org_demo_greenclimatefund', 40),
    c('org_demo_africanunion', 35),
    c('org_demo_climate_funds', 25)
  ]);
  addContributors(trees.afdb, 'afdb_root_energy', [
    c('org_demo_irena', 45),
    c('org_demo_isa', 40),
    c('org_demo_undp', 30)
  ]);
}

// IADB - Inter-American Development Bank
if (trees.iadb) {
  addContributors(trees.iadb, 'iadb_root_finance', [
    c('org_demo_worldbank', 45),
    c('org_demo_greenclimatefund', 40),
    c('org_demo_caf', 35),
    c('org_demo_climate_funds', 25)
  ]);
  addContributors(trees.iadb, 'iadb_root_renewable', [
    c('org_demo_irena', 45),
    c('org_demo_isa', 35),
    c('org_demo_brazil', 30)
  ]);
}

// CAF - Development Bank of Latin America
if (trees.caf) {
  addContributors(trees.caf, 'caf_root_finance', [
    c('org_demo_iadb', 45),
    c('org_demo_worldbank', 40),
    c('org_demo_greenclimatefund', 35)
  ]);
  addContributors(trees.caf, 'caf_root_infrastructure', [
    c('org_demo_cdri', 40),
    c('org_demo_worldbank', 35)
  ]);
}

// KFW - German Development Finance
if (trees.kfw) {
  addContributors(trees.kfw, 'kfw_root_development_finance', [
    c('org_demo_germany', 50),
    c('org_demo_greenclimatefund', 45),
    c('org_demo_worldbank', 40),
    c('org_demo_climate_funds', 30)
  ]);
  addContributors(trees.kfw, 'kfw_root_innovation', [
    c('org_demo_irena', 40),
    c('org_demo_gatesfoundation', 30)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCE AFRICAN COUNTRIES
// ═══════════════════════════════════════════════════════════════════

const africanCountries = ['namibia', 'tanzania', 'liberia', 'sierra_leone', 'cote_ivoire',
  'mali', 'malawi', 'djibouti', 'congo_drc', 'senegal', 'chad', 'rwanda',
  'gabon', 'angola', 'guinea', 'mauritania', 'uganda'];

africanCountries.forEach(country => {
  if (trees[country]) {
    // Add common African contributors
    const rootKeys = Object.keys(trees[country].tree.children || []);
    if (rootKeys.length > 0) {
      const firstPriority = trees[country].tree.children[0];
      if (firstPriority && firstPriority.contributors) {
        firstPriority.contributors = [
          c('org_demo_africandevbank', 40),
          c('org_demo_africanunion', 35),
          c('org_demo_undp', 30),
          c('org_demo_worldbank', 25)
        ];
      }
    }
  }
});

// ═══════════════════════════════════════════════════════════════════
// ENHANCE ASIAN COUNTRIES
// ═══════════════════════════════════════════════════════════════════

const asianCountries = ['pakistan', 'bangladesh', 'thailand', 'malaysia', 'singapore'];

asianCountries.forEach(country => {
  if (trees[country]) {
    const firstPriority = trees[country].tree.children[0];
    if (firstPriority && firstPriority.contributors) {
      firstPriority.contributors = [
        c('org_demo_asiandevbank', 40),
        c('org_demo_undp', 35),
        c('org_demo_worldbank', 30)
      ];
    }
  }
});

// ═══════════════════════════════════════════════════════════════════
// ENHANCE LATIN AMERICAN COUNTRIES
// ═══════════════════════════════════════════════════════════════════

const latinCountries = ['peru', 'colombia', 'el_salvador', 'uruguay', 'venezuela'];

latinCountries.forEach(country => {
  if (trees[country]) {
    const firstPriority = trees[country].tree.children[0];
    if (firstPriority && firstPriority.contributors) {
      firstPriority.contributors = [
        c('org_demo_iadb', 40),
        c('org_demo_caf', 35),
        c('org_demo_undp', 30),
        c('org_demo_worldbank', 25)
      ];
    }
  }
});

// ═══════════════════════════════════════════════════════════════════
// ENHANCE NGOS AND CIVIL SOCIETY
// ═══════════════════════════════════════════════════════════════════

// CAN - Climate Action Network International
if (trees.can) {
  addContributors(trees.can, 'can_root_advocacy', [
    c('org_demo_unfccc', 45),
    c('org_demo_climateactionnetwork', 40),
    c('org_demo_oxfam', 35),
    c('org_demo_greenpeace', 30)
  ]);
  addContributors(trees.can, 'can_root_fossil', [
    c('org_demo_threefiveozero', 45),
    c('org_demo_greenpeace', 40)
  ]);
}

// Indigenous Pavilion
if (trees.indigenous_pavilion) {
  addContributors(trees.indigenous_pavilion, 'indigenous_pavilion_root_rights', [
    c('org_demo_conservationinternational', 45),
    c('org_demo_wwf', 40),
    c('org_demo_cfrn', 35),
    c('org_demo_wipo', 30)
  ]);
  addContributors(trees.indigenous_pavilion, 'indigenous_pavilion_root_conservation', [
    c('org_demo_wwf', 45),
    c('org_demo_natureconservancy', 40),
    c('org_demo_fao', 30)
  ]);
}

// IPAM - Amazon Environmental Research
if (trees.ipam) {
  addContributors(trees.ipam, 'ipam_root_research', [
    c('org_demo_fao', 45),
    c('org_demo_ipcc', 40),
    c('org_demo_brazil', 35),
    c('org_demo_conservationinternational', 30)
  ]);
  addContributors(trees.ipam, 'ipam_root_community', [
    c('org_demo_wwf', 40),
    c('org_demo_indigenous_pavilion', 35)
  ]);
}

// LDC Group
if (trees.ldc_group) {
  addContributors(trees.ldc_group, 'ldc_group_root_advocacy', [
    c('org_demo_unfccc', 50),
    c('org_demo_undp', 45),
    c('org_demo_aosis', 35)
  ]);
  addContributors(trees.ldc_group, 'ldc_group_root_adaptation', [
    c('org_demo_greenclimatefund', 50),
    c('org_demo_worldbank', 40),
    c('org_demo_undp', 35)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCE MIDDLE EAST COUNTRIES
// ═══════════════════════════════════════════════════════════════════

// Saudi Arabia
if (trees.saudi_arabia) {
  addContributors(trees.saudi_arabia, 'saudi_arabia_root_circular_carbon', [
    c('org_demo_irena', 45),
    c('org_demo_isa', 40),
    c('org_demo_ipcc', 30)
  ]);
}

// UAE (if exists)
// Qatar
if (trees.qatar) {
  addContributors(trees.qatar, 'qatar_root_innovation', [
    c('org_demo_irena', 40),
    c('org_demo_ipcc', 35),
    c('org_demo_unfccc', 30)
  ]);
}

// Oman
if (trees.oman) {
  addContributors(trees.oman, 'oman_root_hydrogen', [
    c('org_demo_irena', 50),
    c('org_demo_germany', 40),
    c('org_demo_chile', 30)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCE EUROPEAN COUNTRIES
// ═══════════════════════════════════════════════════════════════════

const europeanCountries = ['sweden', 'finland', 'spain', 'portugal', 'italy',
  'luxembourg', 'iceland', 'ukraine'];

europeanCountries.forEach(country => {
  if (trees[country]) {
    const firstPriority = trees[country].tree.children[0];
    if (firstPriority && firstPriority.contributors) {
      firstPriority.contributors = [
        c('org_demo_europeanunion', 45),
        c('org_demo_unep', 35),
        c('org_demo_irena', 30)
      ];
    }
  }
});

// ═══════════════════════════════════════════════════════════════════
// ENHANCE RESEARCH & ACADEMIC INSTITUTIONS
// ═══════════════════════════════════════════════════════════════════

// Tsinghua University
if (trees.tsinghua) {
  addContributors(trees.tsinghua, 'tsinghua_root_research', [
    c('org_demo_ipcc', 45),
    c('org_demo_china', 40),
    c('org_demo_unfccc', 30)
  ]);
  addContributors(trees.tsinghua, 'tsinghua_root_innovation', [
    c('org_demo_irena', 40),
    c('org_demo_china', 35)
  ]);
}

// Tecnológico de Monterrey
if (trees.monterrey) {
  addContributors(trees.monterrey, 'monterrey_root_research', [
    c('org_demo_ipcc', 40),
    c('org_demo_iadb', 35),
    c('org_demo_irena', 30)
  ]);
}

// DEVAL
if (trees.deval) {
  addContributors(trees.deval, 'deval_root_evaluation', [
    c('org_demo_germany', 45),
    c('org_demo_undp', 40),
    c('org_demo_worldbank', 35)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// ENHANCE YOUTH ORGANIZATIONS
// ═══════════════════════════════════════════════════════════════════

const youthOrgs = ['yle', 'iync', 'ycla', 'children_youth_pavilion', 'yilaa'];

youthOrgs.forEach(org => {
  if (trees[org]) {
    const firstPriority = trees[org].tree.children[0];
    if (firstPriority && firstPriority.contributors) {
      firstPriority.contributors = [
        c('org_demo_unicef', 40),
        c('org_demo_undp', 35),
        c('org_demo_unep', 25)
      ];
    }
  }
});

// ═══════════════════════════════════════════════════════════════════
// ENHANCE REGIONAL ORGANIZATIONS
// ═══════════════════════════════════════════════════════════════════

// East African Community
if (trees.eac) {
  addContributors(trees.eac, 'eac_root_regional_action', [
    c('org_demo_africanunion', 45),
    c('org_demo_africandevbank', 40),
    c('org_demo_undp', 35),
    c('org_demo_ethiopia', 25),
    c('org_demo_tanzania', 25)
  ]);
}

// OIF - Organisation Internationale de la Francophonie
if (trees.oif) {
  addContributors(trees.oif, 'oif_root_cooperation', [
    c('org_demo_france', 45),
    c('org_demo_undp', 40),
    c('org_demo_unfccc', 35)
  ]);
  addContributors(trees.oif, 'oif_root_advocacy', [
    c('org_demo_unfccc', 45),
    c('org_demo_ldc_group', 35)
  ]);
}

// Climate Funds Pavilion
if (trees.climate_funds) {
  addContributors(trees.climate_funds, 'climate_funds_root_coordination', [
    c('org_demo_greenclimatefund', 50),
    c('org_demo_worldbank', 45),
    c('org_demo_unfccc', 40),
    c('org_demo_africandevbank', 30),
    c('org_demo_asiandevbank', 30),
    c('org_demo_iadb', 30)
  ]);
}

// Multilateral Banks Pavilion
if (trees.multilateral_banks) {
  addContributors(trees.multilateral_banks, 'multilateral_banks_root_coordination', [
    c('org_demo_worldbank', 50),
    c('org_demo_africandevbank', 40),
    c('org_demo_asiandevbank', 40),
    c('org_demo_iadb', 40),
    c('org_demo_greenclimatefund', 35)
  ]);
}

// ═══════════════════════════════════════════════════════════════════
// SAVE ENHANCED TREES
// ═══════════════════════════════════════════════════════════════════

fs.writeFileSync('./src/lib/demo/orgs.json', JSON.stringify(trees, null, 2), 'utf8');

console.log('✅ Enhanced all organization trees with contributors!');
console.log('   - Added contributor networks to priority nodes');
console.log('   - Linked organizations based on mandates and partnerships');
console.log('   - Created regional collaboration networks');
console.log('   - Enhanced UN agency coordination');
console.log('   - Strengthened development bank linkages');

// Count enhancements
let contributorCount = 0;
Object.values(trees).forEach(tree => {
  function countContributors(node) {
    if (node.contributors && node.contributors.length > 0) {
      contributorCount += node.contributors.length;
    }
    if (node.children) {
      node.children.forEach(child => countContributors(child));
    }
  }
  if (tree.tree) countContributors(tree.tree);
});

console.log(`   - Total contributor relationships added: ${contributorCount}`);


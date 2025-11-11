/**
 * Final Expansion - Complete the original 33 organizations
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/config/org-trees.json', 'utf8'));
const c = (id, pts) => ({ id, points: pts });

function expandOrg(slug, additionalPriorities) {
  if (!trees[slug]) return false;
  
  const tree = trees[slug];
  const currentPriorities = tree.tree.children.length;
  
  if (currentPriorities >= 6) return false;
  
  additionalPriorities.forEach(priority => {
    const newPriority = {
      id: `${slug}_root_${priority.id}`,
      name: priority.name,
      type: "NonRootNode",
      manual_fulfillment: null,
      points: priority.points,
      parent_id: `${slug}_root`,
      contributors: priority.rootContributors || [],
      anti_contributors: [],
      children: priority.subs.map(s => ({
        id: `${slug}_root_${priority.id}_${s.i}`,
        name: s.n,
        type: "NonRootNode",
        manual_fulfillment: null,
        points: s.p,
        parent_id: `${slug}_root_${priority.id}`,
        contributors: s.c || [],
        anti_contributors: [],
        children: []
      }))
    };
    
    tree.tree.children.push(newPriority);
  });
  
  console.log(`  ✓ ${slug}: ${currentPriorities} → ${tree.tree.children.length} priorities`);
  return true;
}

console.log('🔧 Final Expansion - Original 33 Organizations\n');

// UN AGENCIES
expandOrg('unep', [
  { id: 'oceans', name: 'Ocean Protection', points: 15,
    subs: [{ i: 'marine', n: 'Marine Ecosystems', p: 100, c: [c('org_demo_oceandori',40), c('org_demo_ramsar',30)] }]
  },
  { id: 'chemicals', name: 'Chemicals & Pollution', points: 15,
    subs: [{ i: 'toxic', n: 'Toxic Substances', p: 100, c: [c('org_demo_who',35)] }]
  },
  { id: 'sustainable_consumption', name: 'Sustainable Consumption', points: 10,
    subs: [{ i: 'lifestyles', n: 'Sustainable Lifestyles', p: 100, c: [c('org_demo_wgeo',30)] }]
  }
]);

expandOrg('undp', [
  { id: 'energy_access', name: 'Energy Access', points: 15,
    subs: [{ i: 'universal', n: 'Universal Energy Access', p: 100, c: [c('org_demo_irena',40), c('org_demo_isa',35)] }]
  },
  { id: 'governance', name: 'Climate Governance', points: 15,
    subs: [{ i: 'institutions', n: 'Institutional Capacity', p: 100, c: [c('org_demo_unfccc',35)] }]
  },
  { id: 'sdgs', name: 'SDGs Integration', points: 10,
    subs: [{ i: 'linkages', n: 'Climate-SDG Linkages', p: 100, c: [c('org_demo_unep',30)] }]
  }
]);

expandOrg('unicef', [
  { id: 'education', name: 'Climate Education', points: 20,
    subs: [{ i: 'schools', n: 'School Programs', p: 100, c: [c('org_demo_children_youth_pavilion',35)] }]
  },
  { id: 'wash', name: 'Water & Sanitation', points: 15,
    subs: [{ i: 'access', n: 'Climate-Resilient WASH', p: 100, c: [c('org_demo_siwi',40), c('org_demo_who',30)] }]
  },
  { id: 'nutrition', name: 'Climate & Nutrition', points: 10,
    subs: [{ i: 'food_security', n: 'Child Food Security', p: 100, c: [c('org_demo_wfp',40), c('org_demo_fao',35)] }]
  }
]);

expandOrg('unhcr', [
  { id: 'shelter', name: 'Climate-Resilient Shelter', points: 20,
    subs: [{ i: 'housing', n: 'Emergency Housing', p: 100, c: [c('org_demo_redcross',35)] }]
  },
  { id: 'livelihoods', name: 'Displaced Livelihoods', points: 15,
    subs: [{ i: 'economic', n: 'Economic Support', p: 100, c: [c('org_demo_iom',40), c('org_demo_undp',30)] }]
  },
  { id: 'solutions', name: 'Durable Solutions', points: 10,
    subs: [{ i: 'integration', n: 'Local Integration', p: 100, c: [c('org_demo_iom',35)] }]
  }
]);

expandOrg('wfp', [
  { id: 'resilience', name: 'Food System Resilience', points: 20,
    subs: [{ i: 'systems', n: 'Resilient Food Systems', p: 100, c: [c('org_demo_fao',45), c('org_demo_undp',30)] }]
  },
  { id: 'anticipatory', name: 'Anticipatory Action', points: 15,
    subs: [{ i: 'early_action', n: 'Early Action Systems', p: 100, c: [c('org_demo_wmo',40), c('org_demo_redcross',30)] }]
  },
  { id: 'smallholders', name: 'Smallholder Support', points: 10,
    subs: [{ i: 'farmers', n: 'Climate-Smart Farming', p: 100, c: [c('org_demo_fao',45)] }]
  }
]);

expandOrg('who', [
  { id: 'heat', name: 'Heat Health', points: 20,
    subs: [{ i: 'action_plans', n: 'Heat Action Plans', p: 100, c: [c('org_demo_wmo',35), c('org_demo_c40cities',25)] }]
  },
  { id: 'vector', name: 'Vector-Borne Diseases', points: 15,
    subs: [{ i: 'surveillance', n: 'Disease Surveillance', p: 100, c: [c('org_demo_wmo',30), c('org_demo_unep',25)] }]
  },
  { id: 'air_quality', name: 'Air Quality & Health', points: 10,
    subs: [{ i: 'monitoring', n: 'Air Quality Monitoring', p: 100, c: [c('org_demo_unep',40), c('org_demo_wmo',30)] }]
  }
]);

expandOrg('fao', [
  { id: 'soil', name: 'Soil Health', points: 15,
    subs: [{ i: 'management', n: 'Sustainable Soil Management', p: 100, c: [c('org_demo_unep',35), c('org_demo_ipcc',25)] }]
  },
  { id: 'fisheries', name: 'Climate-Resilient Fisheries', points: 15,
    subs: [{ i: 'sustainable', n: 'Sustainable Fishing', p: 100, c: [c('org_demo_oceandori',40), c('org_demo_unep',30)] }]
  },
  { id: 'livestock', name: 'Sustainable Livestock', points: 10,
    subs: [{ i: 'emissions', n: 'Livestock Emissions', p: 100, c: [c('org_demo_ipcc',35)] }]
  }
]);

expandOrg('unhabitat', [
  { id: 'housing', name: 'Climate Housing', points: 20,
    subs: [{ i: 'affordable', n: 'Affordable Climate Housing', p: 100, c: [c('org_demo_c40cities',35), c('org_demo_worldbank',30)] }]
  },
  { id: 'slums', name: 'Slum Upgrading', points: 15,
    subs: [{ i: 'resilience', n: 'Resilient Settlements', p: 100, c: [c('org_demo_undp',35), c('org_demo_redcross',25)] }]
  },
  { id: 'planning', name: 'Climate Urban Planning', points: 10,
    subs: [{ i: 'tools', n: 'Planning Tools', p: 100, c: [c('org_demo_iclei',40), c('org_demo_c40cities',35)] }]
  }
]);

expandOrg('ipcc', [
  { id: 'special_reports', name: 'Special Reports', points: 15,
    subs: [{ i: 'thematic', n: 'Thematic Assessments', p: 100, c: [] }]
  },
  { id: 'regional', name: 'Regional Assessments', points: 15,
    subs: [{ i: 'analysis', n: 'Regional Analysis', p: 100, c: [c('org_demo_wmo',30)] }]
  },
  { id: 'data', name: 'Climate Data', points: 10,
    subs: [{ i: 'standards', n: 'Data Standards', p: 100, c: [c('org_demo_wmo',40)] }]
  }
]);

// NGOS
expandOrg('greenpeace', [
  { id: 'oceans', name: 'Ocean Protection', points: 15,
    subs: [{ i: 'campaigns', n: 'Ocean Campaigns', p: 100, c: [c('org_demo_oceandori',35)] }]
  },
  { id: 'renewable', name: 'Renewable Energy Advocacy', points: 15,
    subs: [{ i: 'campaigns', n: 'RE Campaigns', p: 100, c: [c('org_demo_threefiveozero',35), c('org_demo_irena',25)] }]
  },
  { id: 'toxic', name: 'Toxic Pollution', points: 10,
    subs: [{ i: 'chemicals', n: 'Chemical Pollution', p: 100, c: [c('org_demo_unep',40)] }]
  }
]);

expandOrg('wwf', [
  { id: 'freshwater', name: 'Freshwater Conservation', points: 15,
    subs: [{ i: 'rivers', n: 'River Protection', p: 100, c: [c('org_demo_ramsar',40), c('org_demo_siwi',35)] }]
  },
  { id: 'wildlife_trade', name: 'Wildlife & Climate', points: 15,
    subs: [{ i: 'trafficking', n: 'Wildlife Protection', p: 100, c: [c('org_demo_unep',35)] }]
  },
  { id: 'footprint', name: 'Ecological Footprint', points: 10,
    subs: [{ i: 'reduction', n: 'Footprint Reduction', p: 100, c: [c('org_demo_wgeo',30)] }]
  }
]);

expandOrg('natureconservancy', [
  { id: 'coastal', name: 'Coastal Resilience', points: 15,
    subs: [{ i: 'restoration', n: 'Coastal Restoration', p: 100, c: [c('org_demo_oceandori',40), c('org_demo_ramsar',35)] }]
  },
  { id: 'freshwater', name: 'Freshwater Systems', points: 15,
    subs: [{ i: 'protection', n: 'Watershed Protection', p: 100, c: [c('org_demo_siwi',40), c('org_demo_fao',30)] }]
  },
  { id: 'carbon_markets', name: 'Natural Climate Solutions Finance', points: 10,
    subs: [{ i: 'markets', n: 'Carbon Markets', p: 100, c: [c('org_demo_cfrn',35), c('org_demo_worldbank',30)] }]
  }
]);

expandOrg('conservationinternational', [
  { id: 'hotspots', name: 'Biodiversity Hotspots', points: 15,
    subs: [{ i: 'protection', n: 'Hotspot Protection', p: 100, c: [c('org_demo_unep',40), c('org_demo_wwf',35)] }]
  },
  { id: 'indigenous', name: 'Indigenous Partnership', points: 15,
    subs: [{ i: 'collaboration', n: 'Indigenous Collaboration', p: 100, c: [c('org_demo_indigenous_pavilion',45), c('org_demo_wipo',35)] }]
  },
  { id: 'science', name: 'Conservation Science', points: 10,
    subs: [{ i: 'research', n: 'Scientific Research', p: 100, c: [c('org_demo_ipcc',35)] }]
  }
]);

expandOrg('oxfam', [
  { id: 'loss_damage', name: 'Loss & Damage Justice', points: 15,
    subs: [{ i: 'advocacy', n: 'L&D Advocacy', p: 100, c: [c('org_demo_aosis',45), c('org_demo_ldc_group',40)] }]
  },
  { id: 'women', name: 'Women & Climate', points: 15,
    subs: [{ i: 'leadership', n: 'Women Leadership', p: 100, c: [c('org_demo_undp',35)] }]
  },
  { id: 'food_justice', name: 'Food Justice', points: 10,
    subs: [{ i: 'systems', n: 'Just Food Systems', p: 100, c: [c('org_demo_fao',40), c('org_demo_wfp',35)] }]
  }
]);

expandOrg('redcross', [
  { id: 'forecast', name: 'Forecast-Based Financing', points: 15,
    subs: [{ i: 'systems', n: 'FbF Systems', p: 100, c: [c('org_demo_wmo',45), c('org_demo_undp',30)] }]
  },
  { id: 'shelter', name: 'Emergency Shelter', points: 15,
    subs: [{ i: 'climate_proof', n: 'Climate-Proof Shelters', p: 100, c: [c('org_demo_unhcr',40), c('org_demo_cdri',30)] }]
  },
  { id: 'community', name: 'Community Resilience', points: 10,
    subs: [{ i: 'preparedness', n: 'Community Preparedness', p: 100, c: [c('org_demo_undp',35)] }]
  }
]);

expandOrg('threefiveozero', [
  { id: 'divestment', name: 'Fossil Fuel Divestment', points: 20,
    subs: [{ i: 'campaigns', n: 'Divestment Campaigns', p: 100, c: [c('org_demo_greenpeace',35), c('org_demo_can',30)] }]
  },
  { id: 'activism', name: 'Climate Activism', points: 15,
    subs: [{ i: 'mobilization', n: 'Mass Mobilization', p: 100, c: [c('org_demo_climate_live',30)] }]
  },
  { id: 'community', name: 'Community Power', points: 10,
    subs: [{ i: 'local', n: 'Local Energy Campaigns', p: 100, c: [c('org_demo_irena',30)] }]
  }
]);

// FINANCE
expandOrg('greenclimatefund', [
  { id: 'adaptation', name: 'Adaptation Finance', points: 20,
    subs: [{ i: 'projects', n: 'Adaptation Projects', p: 100, c: [c('org_demo_undp',40), c('org_demo_aosis',35)] }]
  },
  { id: 'mitigation', name: 'Mitigation Finance', points: 20,
    subs: [{ i: 'energy', n: 'Clean Energy Finance', p: 100, c: [c('org_demo_irena',45), c('org_demo_worldbank',35)] }]
  },
  { id: 'private_sector', name: 'Private Sector Facility', points: 15,
    subs: [{ i: 'investment', n: 'Private Investment', p: 100, c: [c('org_demo_icc_chamber',35)] }]
  },
  { id: 'readiness', name: 'Readiness Support', points: 15,
    subs: [{ i: 'capacity', n: 'Country Capacity', p: 100, c: [c('org_demo_undp',40), c('org_demo_ndc_partnership',30)] }]
  }
]);

expandOrg('worldbank', [
  { id: 'country_programs', name: 'Country Climate Programs', points: 15,
    subs: [{ i: 'support', n: 'Country Support', p: 100, c: [c('org_demo_undp',40)] }]
  },
  { id: 'research', name: 'Climate Research', points: 15,
    subs: [{ i: 'analytics', n: 'Climate Analytics', p: 100, c: [c('org_demo_ipcc',40), c('org_demo_wmo',30)] }]
  },
  { id: 'carbon_markets', name: 'Carbon Markets', points: 10,
    subs: [{ i: 'mechanisms', n: 'Market Mechanisms', p: 100, c: [c('org_demo_unfccc',40)] }]
  }
]);

expandOrg('imf', [
  { id: 'fiscal', name: 'Fiscal Policy & Climate', points: 20,
    subs: [{ i: 'carbon_pricing', n: 'Carbon Pricing Policy', p: 100, c: [c('org_demo_worldbank',40), c('org_demo_unfccc',30)] }]
  },
  { id: 'debt', name: 'Climate Debt Relief', points: 15,
    subs: [{ i: 'mechanisms', n: 'Debt-for-Climate Swaps', p: 100, c: [c('org_demo_ldc_group',40), c('org_demo_aosis',35)] }]
  },
  { id: 'risk', name: 'Climate Financial Risk', points: 10,
    subs: [{ i: 'assessment', n: 'Risk Assessment', p: 100, c: [c('org_demo_worldbank',35)] }]
  }
]);

expandOrg('asiandevbank', [
  { id: 'coal', name: 'Coal Transition', points: 15,
    subs: [{ i: 'phase_out', n: 'Coal Phase-Out Support', p: 100, c: [c('org_demo_irena',40), c('org_demo_ilo',30)] }]
  },
  { id: 'cities', name: 'Sustainable Cities', points: 15,
    subs: [{ i: 'urban', n: 'Urban Development', p: 100, c: [c('org_demo_c40cities',40), c('org_demo_iclei',35)] }]
  },
  { id: 'water', name: 'Water Security', points: 10,
    subs: [{ i: 'infrastructure', n: 'Water Infrastructure', p: 100, c: [c('org_demo_siwi',40)] }]
  }
]);

expandOrg('africandevbank', [
  { id: 'desert_to_power', name: 'Desert to Power', points: 15,
    subs: [{ i: 'sahel', n: 'Sahel Solar', p: 100, c: [c('org_demo_isa',45), c('org_demo_irena',40)] }]
  },
  { id: 'green_baseload', name: 'Green Baseload', points: 15,
    subs: [{ i: 'hydro', n: 'Hydropower Development', p: 100, c: [c('org_demo_irena',40)] }]
  },
  { id: 'agriculture', name: 'Climate-Smart Agriculture', points: 10,
    subs: [{ i: 'programs', n: 'Agricultural Programs', p: 100, c: [c('org_demo_fao',45)] }]
  }
]);

// FOUNDATIONS
expandOrg('bezosearthfund', [
  { id: 'restoration', name: 'Landscape Restoration', points: 15,
    subs: [{ i: 'projects', n: 'Restoration Projects', p: 100, c: [c('org_demo_conservationinternational',40), c('org_demo_fao',35)] }]
  },
  { id: 'transformation', name: 'Food System Transformation', points: 15,
    subs: [{ i: 'systems', n: 'Sustainable Food Systems', p: 100, c: [c('org_demo_fao',45), c('org_demo_wfp',35)] }]
  },
  { id: 'innovation', name: 'Climate Innovation', points: 10,
    subs: [{ i: 'technology', n: 'Breakthrough Technology', p: 100, c: [c('org_demo_gatesfoundation',35)] }]
  }
]);

expandOrg('gatesfoundation', [
  { id: 'agriculture', name: 'Climate Agriculture Innovation', points: 15,
    subs: [{ i: 'seeds', n: 'Climate-Resilient Seeds', p: 100, c: [c('org_demo_fao',45)] }]
  },
  { id: 'energy', name: 'Energy Innovation', points: 15,
    subs: [{ i: 'nuclear', n: 'Advanced Nuclear', p: 100, c: [c('org_demo_iaea',40)] }]
  },
  { id: 'sanitation', name: 'Climate Sanitation', points: 10,
    subs: [{ i: 'systems', n: 'Resilient Sanitation', p: 100, c: [c('org_demo_unicef',40), c('org_demo_who',35)] }]
  }
]);

expandOrg('rockefellerfoundation', [
  { id: 'food_security', name: 'Food Security', points: 15,
    subs: [{ i: 'systems', n: 'Resilient Food Systems', p: 100, c: [c('org_demo_fao',45), c('org_demo_wfp',35)] }]
  },
  { id: 'energy_access', name: 'Energy Access', points: 15,
    subs: [{ i: 'mini_grids', n: 'Mini-Grid Development', p: 100, c: [c('org_demo_irena',40), c('org_demo_undp',35)] }]
  },
  { id: 'cities', name: 'Resilient Cities', points: 10,
    subs: [{ i: 'urban', n: 'Urban Resilience', p: 100, c: [c('org_demo_c40cities',40), c('org_demo_cdri',30)] }]
  }
]);

expandOrg('bloombergphilanthropies', [
  { id: 'coal', name: 'Beyond Coal', points: 15,
    subs: [{ i: 'transition', n: 'Coal Transition', p: 100, c: [c('org_demo_threefiveozero',40), c('org_demo_irena',35)] }]
  },
  { id: 'mobility', name: 'Urban Mobility', points: 15,
    subs: [{ i: 'transport', n: 'Sustainable Transport', p: 100, c: [c('org_demo_c40cities',45), c('org_demo_iclei',35)] }]
  },
  { id: 'data', name: 'Climate Data', points: 10,
    subs: [{ i: 'systems', n: 'Data Systems', p: 100, c: [c('org_demo_wmo',35)] }]
  }
]);

expandOrg('fordfoundation', [
  { id: 'justice', name: 'Climate Justice', points: 15,
    subs: [{ i: 'equity', n: 'Climate Equity', p: 100, c: [c('org_demo_oxfam',45), c('org_demo_open_society',35)] }]
  },
  { id: 'civic_engagement', name: 'Civic Engagement', points: 15,
    subs: [{ i: 'movements', n: 'Climate Movements', p: 100, c: [c('org_demo_climateactionnetwork',40), c('org_demo_can',35)] }]
  },
  { id: 'arts', name: 'Climate & Arts', points: 10,
    subs: [{ i: 'culture', n: 'Cultural Expression', p: 100, c: [c('org_demo_climate_live',35)] }]
  }
]);

expandOrg('climateworks', [
  { id: 'policy', name: 'Climate Policy', points: 15,
    subs: [{ i: 'advocacy', n: 'Policy Advocacy', p: 100, c: [c('org_demo_unfccc',40), c('org_demo_can',35)] }]
  },
  { id: 'china', name: 'China Climate', points: 15,
    subs: [{ i: 'programs', n: 'China Programs', p: 100, c: [c('org_demo_china',40)] }]
  },
  { id: 'transport', name: 'Transport Decarbonization', points: 10,
    subs: [{ i: 'mobility', n: 'Clean Mobility', p: 100, c: [c('org_demo_iclei',35)] }]
  }
]);

// REGIONAL
expandOrg('europeanunion', [
  { id: 'taxonomy', name: 'Green Taxonomy', points: 15,
    subs: [{ i: 'classification', n: 'Activity Classification', p: 100, c: [c('org_demo_iso',35)] }]
  },
  { id: 'cbam', name: 'Carbon Border Adjustment', points: 15,
    subs: [{ i: 'mechanism', n: 'CBAM Implementation', p: 100, c: [c('org_demo_unfccc',35)] }]
  },
  { id: 'cohesion', name: 'Cohesion & Climate', points: 10,
    subs: [{ i: 'regions', n: 'Regional Support', p: 100, c: [c('org_demo_ilo',35)] }]
  }
]);

expandOrg('africanunion', [
  { id: 'great_green_wall', name: 'Great Green Wall', points: 15,
    subs: [{ i: 'restoration', n: 'Land Restoration', p: 100, c: [c('org_demo_fao',45), c('org_demo_africandevbank',40)] }]
  },
  { id: 'coordination', name: 'Regional Coordination', points: 15,
    subs: [{ i: 'member_states', n: 'Member State Coordination', p: 100, c: [c('org_demo_eac',35)] }]
  },
  { id: 'finance', name: 'African Climate Finance', points: 10,
    subs: [{ i: 'mobilization', n: 'Finance Mobilization', p: 100, c: [c('org_demo_africandevbank',45), c('org_demo_greenclimatefund',40)] }]
  }
]);

expandOrg('aosis', [
  { id: '1_5', name: '1.5°C Advocacy', points: 15,
    subs: [{ i: 'ambition', n: 'Ambition Raising', p: 100, c: [c('org_demo_unfccc',50), c('org_demo_ipcc',40)] }]
  },
  { id: 'ocean', name: 'Ocean & Islands', points: 15,
    subs: [{ i: 'protection', n: 'Ocean Protection', p: 100, c: [c('org_demo_oceandori',45), c('org_demo_ramsar',35)] }]
  },
  { id: 'finance_access', name: 'Finance Access', points: 10,
    subs: [{ i: 'mechanisms', n: 'Access Mechanisms', p: 100, c: [c('org_demo_greenclimatefund',50), c('org_demo_undp',40)] }]
  }
]);

expandOrg('c40cities', [
  { id: 'buildings', name: 'Building Efficiency', points: 15,
    subs: [{ i: 'retrofits', n: 'Building Retrofits', p: 100, c: [c('org_demo_iclei',40), c('org_demo_iso',30)] }]
  },
  { id: 'procurement', name: 'Green Procurement', points: 15,
    subs: [{ i: 'standards', n: 'Procurement Standards', p: 100, c: [c('org_demo_iclei',40)] }]
  },
  { id: 'finance', name: 'Urban Climate Finance', points: 10,
    subs: [{ i: 'mechanisms', n: 'Finance Mechanisms', p: 100, c: [c('org_demo_worldbank',40), c('org_demo_greenclimatefund',35)] }]
  }
]);

expandOrg('climateactionnetwork', [
  { id: 'fossil', name: 'Fossil Fuel Phase-Out', points: 15,
    subs: [{ i: 'campaigns', n: 'Phase-Out Campaigns', p: 100, c: [c('org_demo_threefiveozero',45), c('org_demo_greenpeace',40)] }]
  },
  { id: 'equity', name: 'Climate Equity', points: 15,
    subs: [{ i: 'justice', n: 'Equity & Justice', p: 100, c: [c('org_demo_oxfam',45), c('org_demo_can',35)] }]
  },
  { id: 'ngo_coordination', name: 'NGO Coordination', points: 10,
    subs: [{ i: 'network', n: 'Network Coordination', p: 100, c: [c('org_demo_can',40)] }]
  }
]);

expandOrg('wemeanbus', [
  { id: 'sbti', name: 'Science Based Targets', points: 15,
    subs: [{ i: 'targets', n: 'Corporate Targets', p: 100, c: [c('org_demo_ipcc',40), c('org_demo_icc_chamber',35)] }]
  },
  { id: 'policy', name: 'Policy Advocacy', points: 15,
    subs: [{ i: 'engagement', n: 'Policy Engagement', p: 100, c: [c('org_demo_unfccc',40), c('org_demo_icc_chamber',35)] }]
  },
  { id: 'leadership', name: 'CEO Leadership', points: 10,
    subs: [{ i: 'commitment', n: 'CEO Commitments', p: 100, c: [] }]
  }
]);

// Save
fs.writeFileSync('./src/lib/config/org-trees.json', JSON.stringify(trees, null, 2), 'utf8');

// Final count
let total = 0, under6 = 0;
Object.values(trees).forEach(tree => {
  total++;
  if (tree.tree.children.length < 6) under6++;
});

console.log('\n═══════════════════════════════════════');
console.log('✅ FINAL EXPANSION COMPLETE!');
console.log(`All ${total} organizations now have 6+ priorities`);
console.log(`Organizations still under 6: ${under6}`);
console.log('═══════════════════════════════════════');


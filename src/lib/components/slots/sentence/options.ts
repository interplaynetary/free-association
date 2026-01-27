
export const DAY_OPTIONS = [
    { label: 'Every Day', value: 'all' },
    { label: 'Weekdays (Mon-Fri)', value: 'weekdays' },
    { label: 'Weekends (Sat-Sun)', value: 'weekends' },
    { label: 'Mondays', value: 'mon' },
    { label: 'Tuesdays', value: 'tue' },
    { label: 'Wednesdays', value: 'wed' },
    { label: 'Thursdays', value: 'thu' },
    { label: 'Fridays', value: 'fri' },
    { label: 'Saturdays', value: 'sat' },
    { label: 'Sundays', value: 'sun' },
    { divider: true, label: '', value: '' },
    { label: 'Custom...', value: 'custom' }
];

export const TIME_OPTIONS = [
    { label: 'All Day (9am - 5pm)', value: 'business_hours' }, // Default business assumption
    { label: 'All Day (24 hours)', value: '24_hours' },
    { label: 'Mornings (8am - 12pm)', value: 'mornings' },
    { label: 'Afternoons (1pm - 5pm)', value: 'afternoons' },
    { label: 'Evenings (6pm - 10pm)', value: 'evenings' },
    { divider: true, label: '', value: '' },
    { label: 'Custom Time...', value: 'custom' }
];

export const MONTH_OPTIONS = [
    { label: 'All Year', value: 'all' },
    { label: 'Summer (Jun-Aug)', value: 'summer' },
    { label: 'Winter (Dec-Feb)', value: 'winter' },
    { label: 'Q1 (Jan-Mar)', value: 'q1' },
    { label: 'Q2 (Apr-Jun)', value: 'q2' },
    { label: 'Q3 (Jul-Sep)', value: 'q3' },
    { label: 'Q4 (Oct-Dec)', value: 'q4' },
    { divider: true, label: '', value: '' },
    { label: 'Specific Months...', value: 'custom' }
];

export const WEEK_OPTIONS = [
    { label: 'Every Week', value: 'all' },
    { label: '1st & 3rd Weeks', value: '1st_3rd' },
    { label: '2nd & 4th Weeks', value: '2nd_4th' },
    { divider: true, label: '', value: '' },
    { label: 'Specific Weeks...', value: 'custom' }
];

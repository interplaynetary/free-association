import type { PageLoad } from './$types';

export const load: PageLoad = async ({ params }: { params: { soul: string } }) => {
	const soul = params.soul;
	return { soul };
};

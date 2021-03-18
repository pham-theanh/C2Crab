""" Crab encoding for bitvector programs.

Notes:
  - no global variables in the input program
  - no function definition except for main() (use inliner.py if necessary)
  - no loops (for now) (use unroller.py if necessary)
  - each variable has a unique identifier (hence, no variable shadowing)
  - no control-flow statement except if..then..else (no goto)

  - Ignore typecasting
"""

import core.module
import pycparserext.ext_c_parser, pycparser.c_ast, pycparser.c_generator, pycparserext.ext_c_generator


class crab(core.module.Translator, pycparser.c_generator.CGenerator):
    if_list = []
    block_declareration = ''
    variable_declareration =''
    if_blocks = []
    last_ifelse_blocks = []
    last_state_block = 'entry'
    #exit_parents = 'entry >> exit; \n';
    arrays = []
    enc = ''  # our Crab encoding as a string
    index = 0
    ter_block =0
    bit_wide_dict ={}
    bounds_dict={}
    max_wide = 62
    last_if_true_block= ''
    last_if_false_block = ''
    header =  '#include <crab/config.h> \n'
    header += '#include <crab/common/types.hpp>\n'
    header += '#include <crab/common/debug.hpp>\n'
    header += '#include <crab/cfg/cfg.hpp>\n'
    header += '#include <crab/cfg/var_factory.hpp>\n'
    header += '#include <crab/domains/linear_constraints.hpp>\n'
    header += '#include <crab/domains/array_adaptive.hpp>\n'
    header += '#include <iostream>\n';

    header += '#include <crab/domains/intervals.hpp>\n'
    header += '#include <crab/domains/split_dbm.hpp>\n'
    header += '#include <crab/domains/flat_boolean_domain.hpp>\n'
    header += '// Analysis\n'
    header += '#include <crab/analysis/fwd_analyzer.hpp>\n'

    header += 'using namespace  crab;\n'
    header += 'using namespace crab::domains;\n'
    header += 'using namespace crab::cfg;\n'
    header += 'using namespace crab::analyzer;\n'
    header += 'using namespace ikos;\n'
    header += 'using namespace std;\n'
    header += 'typedef var_factory_impl::str_variable_factory  variable_factory_t;\n'
    header += 'typedef typename variable_factory_t::varname_t   varname_t;\n'
    header += 'typedef std::string  basic_block_label_t;\n'

    header += 'namespace  crab { namespace  cfg_impl  \n {  template <> inline  std::string  get_label_str(std::string  e) { return e; }  } }\n'

    header += '//To  define   CFG   over   integers\n'
    header += 'typedef  crab::cfg::cfg < basic_block_label_t, varname_t, z_number > cfg_t;\n'
    header += 'typedef  cfg_ref < cfg_t > cfg_ref_t;\n'
    header += 'typedef cfg_t::basic_block_t  basic_block_t;\n'
    header += 'typedef variable < z_number, varname_t > var_t;\n'
    header += 'typedef linear_expression < z_number, varname_t > lin_exp_t;\n'
    header += 'typedef linear_constraint < z_number, varname_t > lin_cst_t;\n'
    header += 'typedef linear_constraint_system < z_number, varname_t > lin_cst_sys_t;\n'
    header += 'typedef interval_domain < z_number, varname_t > interval_domain_t;\n'
    header += 'typedef SplitDBM < z_number, varname_t > zones_domain_t;\n'
    header += 'typedef intra_fwd_analyzer < cfg_ref_t, interval_domain_t > interval_analysis_t;\n'
    header += 'typedef intra_fwd_analyzer < cfg_ref_t, zones_domain_t > zones_analysis_t;\n'
    header += 'class ArrayAdaptParams {\npublic:\n  enum  {is_smashable = 1}; \n    enum {smash_at_nonzero_offset = 0}; \n  enum  {max_smashable_cells = 64}; \n    enum {max_array_size = 512}; \n };\n'
    header += 'typedef interval_domain < z_number, varname_t > z_interval_domain_t;\n'
    header += 'typedef array_adaptive_domain < z_interval_domain_t, ArrayAdaptParams > z_aa_int_t;\n'

    header += 'typedef flat_boolean_numerical_domain < z_interval_domain_t > z_bool_interval_domain_t;\n'
    header += 'typedef array_adaptive_domain < z_bool_interval_domain_t, ArrayAdaptParams > z_aa_bool_int_t;\n'
    header += 'typedef intra_fwd_analyzer < cfg_ref_t, z_aa_int_t > z_aa_int_analysis_t;\n'
    header += 'typedef intra_fwd_analyzer < cfg_ref_t, z_aa_bool_int_t > z_aa_bool_analysis_t;\n'

    header += 'int main(int argc, char ** argv) \n'
    header += '{ variable_factory_t vfac; \n'
    footer = 'outs() << prog << "  "; cout << endl << endl; \n'
    footer += '{   z_aa_int_analysis_t ianalyzer(prog);\n'
    footer += 'ianalyzer.run(); cout << endl;\n'
    footer += 'outs() << "--> z_aa_int_analysis_t----------------------------------------------------------------------- ";  cout << endl;\n'
    footer += 'for (auto& bb : prog) { \n'
    footer += 'std::string bb_name = bb.label();\n'
    footer += 'auto inv = ianalyzer.get_pre(bb_name); \n'
    footer += 'outs() << bb_name << ":" << inv << "   ";  cout << endl<< endl;\n'
    footer += '    } \n} \n'
    footer += '{   z_aa_bool_analysis_t ianalyzer(prog);\n'
    footer += 'ianalyzer.run(); cout << endl; \n'
    footer += 'outs() << "--> z_aa_bool_analysis_t ---------------------------------------------------------------------"; cout << endl;\n'
    footer += 'for (auto& bb : prog) { \n'
    footer += 'std::string bb_name = bb.label();\n'
    footer += 'auto inv = ianalyzer.get_pre(bb_name); \n'
    footer += 'outs() << bb_name << ":" << inv << "  ";  cout << endl<< endl;\n'
    footer += '    } \n } \n'
    footer += '{ zones_analysis_t ianalyzer(prog);\n'
    footer += 'ianalyzer.run(); cout << endl;\n'
    footer += 'outs() << "--> zones_analysis_t -------------------------------------------------------------------------"; cout << endl;\n'
    footer += 'for (auto& bb : prog) { \n'
    footer += 'std::string bb_name = bb.label();\n'
    footer += 'auto inv = ianalyzer.get_pre(bb_name); \n'
    footer += 'outs() << bb_name << ":" << inv << "    ";  cout << endl << endl;\n'
    footer += '    } \n} \n'
    footer += '{interval_analysis_t ianalyzer(prog);\n'
    footer += 'ianalyzer.run(); cout << endl;\n'
    footer += 'outs() << "--> interval_analysis_t ----------------------------------------------------------------------"; cout << endl;\n'
    footer += 'for (auto& bb : prog) { \n'
    footer += 'std::string bb_name = bb.label();\n'
    footer += 'auto inv = ianalyzer.get_pre(bb_name); \n'
    footer += 'outs() << bb_name << ":" << inv << "   "; cout << endl <<endl; \n'
    footer += '    } \n } \n return 0; \n }\n'

    def loadfromstring (self, string, env):
        super(self.__class__, self).loadfromstring(string, env)

        self.block_declareration += '%s >>exit;\n' % (self.last_state_block)

        self.output = self.header + self.block_declareration +'\n' + self.variable_declareration + self.enc + self.footer


    def visit_Compound (self, n):

        if (self.blockid == '0'):
            self.block_declareration += 'cfg_t prog("entry", "exit", ARR);\n'
            self.block_declareration += 'basic_block_t& entry = prog.insert("entry");\n'
            self.block_declareration += 'basic_block_t& entry = prog.insert("block_0_0_0");\n'
            self.block_declareration += 'entry >> block_0_0_0;\n'
            self.last_state_block = 'block_0_0_0';
            self.block_declareration += 'basic_block_t& exit = prog.insert("exit");\n'
        x = super(self.__class__, self).visit_Compound(n)

        return x
    # since Crab only handles constant with bit_wide <=62 so we need this function to compute bounds for variables such that their bounds > that limit.
    def computing_bounds(self,x, bit_wide):
        if (bit_wide <=self.max_wide):
            bound = pow(2, bit_wide)
            self.bounds_dict[x] = bound

        else:
            self.variable_declareration += 'var_t bound_for_%s(vfac["bound_for_%s"], crab::INT_TYPE, 32);\n' % (x, x)
            nbloop = int(bit_wide) - self.max_wide;
            maxint = pow(2,self.max_wide)
            self.variable_declareration += 'entry.assign(bound_for_%s,%s);\n'%(x,maxint)
            self.variable_declareration += 'for (int i =0; i <= %s; i++) entry.mul(bound_for_%s,bound_for_%s,2);\n' % (
                nbloop, x, x)
            self.variable_declareration += 'entry.assume(%s < bound_for_%s);\n\n' % (x, x)
            bound = 'bound_for_%s'%x
            self.bounds_dict[x] = bound



    #this function is used to set bounds for arrays.
    def set_bounds (self, variable, bit_wide):
        bounds =0
        if (bit_wide <= self.max_wide):
            bound = pow(2, bit_wide)
            bound = bound - 1

            self.variable_declareration += '\nentry.assume(%s + %s >=0);\n' % (variable, bound)
            self.variable_declareration += 'entry.assume(%s <= %s);\n' % (variable, bound)
            self.bounds_dict[variable] = bound

        else:
            self.variable_declareration += 'var_t bound_for_%s(vfac["bound_for_%s"], crab::INT_TYPE, 32);\n' % (variable, variable)

            nbloop = bit_wide - self.max_wide;
            maxint = pow(2,self.max_wide)
            self.variable_declareration += 'entry.assign(bound_for_%s,%s);\n'%(variable,maxint)
            self.variable_declareration += 'for (int i =0; i < %s; i++) entry.mul(bound_for_%s,bound_for_%s,2);\n' % (
                nbloop, variable, variable)
            self.variable_declareration += 'entry.sub(bound_for_%s,bound_for_%s,1);\n'%(variable, variable)
            bound = 'bound_for_%s'%variable
            self.variable_declareration+=  '\nentry.assume(%s + %s >=0);\n' % (variable, bound)
            self.variable_declareration += 'entry.assume( bound_for_%s >= %s);\n\n' % (variable, variable)
            self.bounds_dict[variable] = bound
        return bounds

    def visit_Decl (self, n):

        lookup = self.Parser.blockdefid(self.blockid, n.name)
        if lookup is None and n.name != 'main':  # function
            self.error("functions are not allowed", snippet=True)
        elif lookup is None and n.name == 'main':
            pass
        elif lookup is '0':  # global variable
            self.error("global variables not allowed", snippet=True)
        else:
            # print('block id =', self.blockid)
            # At this point we are dealing with a variable declaration.
            # 1. if the variable is a bitvector array, we must make bounds for it
            if isinstance(n.type, pycparser.c_ast.ArrayDecl):
                self.arrays.append(n.name);

                self.variable_declareration += 'var_t %s(vfac["%s"], crab::ARR_INT_TYPE, 32);\n' % (n.name, n.name)
                self.variable_declareration += 'var_t tmp_%s(vfac["tmp_%s"], crab::INT_TYPE, 32);\n' % (n.name, n.name)
                if 'bitvector' in n.type.type.type.names[0]:
                    self.variable_declareration += 'var_t bound_for_%s(vfac["bound_for_%s"], crab::INT_TYPE, 32);\n' % (
                        n.name, n.name)
                    arr = 'bound_for_%s' % n.name
                    self.set_bounds(arr, int(n.type.type.type.bit_size)-1)

                    self.variable_declareration += 'entry.array_init(%s,0,%s,%s,1);\n\n' % (n.name, n.type.dim.value, arr)

            else:  # if it is not a array, create a z_variable
                self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32); \n' % (n.name, n.name)
                #e.g,  x = a;
                if (isinstance(n.init,pycparser.c_ast.Constant)):
                    self.variable_declareration += '\nentry.assign(%s,%s);\n' % (n.name,n.init.value)
                # e.g int x = (int) a;
                elif(isinstance(n.init,pycparser.c_ast.Cast)):
                    self.variable_declareration += '\nentry.assign(%s,%s);\n' % (n.name,n.init.expr.value)
                # e.g int x = {0};
                elif (isinstance(n.init,pycparser.c_ast.InitList)):
                    self.variable_declareration += '\nentry.assign(%s,%s);\n' % (n.name,n.init.exprs[0].value)
                if 'bitvector' in n.type.type.names[0]:
                    self.bit_wide_dict[n.name] = int(n.type.type.bit_size)-1
                    self.set_bounds(n.name, int(n.type.type.bit_size)-1)
                else:
                    # -1 ----> not bit_wide, e.g, int a;
                    self.bit_wide_dict[n.name] = -1
        return ''

    def arr_assignt_bound(self,arr,index):
        self.enc+='%s.array_load(tmp_%s, %s, %s, 1);\n'%(self.last_state_block,arr,arr,index);
        key = 'bound_for_%s'%arr
        bound = self.bounds_dict[key]

        #print 'bound for array ============================', self.bounds_dict[arr]
        self.enc+= '%s.bitwise_and(tmp_%s,tmp_%s,%s);\n' % (self.last_state_block, arr,arr,bound)
        self.enc+='%s.array_store(%s, %s, tmp_%s, 1);\n'%(self.last_state_block,arr,index,arr);


     # A simple solution is: after any assignment we apply bitwise_and on the left side of the assignment.
    def scala_assignt_bound(self, x):
        if (int(self.bit_wide_dict[x])==-1):
            return
        if int(self.bit_wide_dict[x]) > self.max_wide:
            bound = self.bounds_dict[x]
        else:
            bound = pow(2,int(self.bit_wide_dict[x]))-1
        self.enc+= '%s.bitwise_and(%s,%s,%s);\n' % (self.last_state_block, x,x,bound)

        '''
        fbl = 'ter_block_%s_f' % self.ter_block
        tbl = 'ter_block_%s_t' % self.ter_block

        self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (tbl, tbl)
        self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (fbl, fbl)
        self.block_declareration += '%s >> %s;\n' % (self.last_state_block, fbl)
        self.block_declareration += '%s >> %s;\n' % (self.last_state_block, tbl)

        if int(self.bit_wide_dict[x]) > self.max_wide:
            bound = self.bounds_dict[x]
        else:
            bound = pow(2,int(self.bit_wide_dict[x]))-1
        bl1 = '%s.assume(%s >= %s);\n' % (tbl,  bound,x)
        bl2 = '%s.assume(%s > %s);\n' % (fbl, x,bound) + '%s.bitwise_and(%s,%s); \n' % (fbl, x,bound)
        self.if_level = self.if_level + 1;
        new_block = 'block_%s_%s_%s' % (self.if_nb, self.if_level, self.else_level)
        self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (new_block, new_block)
        self.new_parents = [new_block]
        self.exit_parents = new_block + ' >> exit;\n'
        self.block_declareration += '%s >> %s;\n' % (tbl, new_block)
        self.block_declareration += '%s >> %s;\n' % (fbl, new_block)
        self.last_state_block = new_block
        self.enc = self.enc + bl1 + bl2
        self.ter_block = self.ter_block + 1
        '''
    def encode_basicAssignment (self, x, y, z, op):
        stm =''
        if (op == '+'):
            stm= '%s.add(%s,%s,%s);\n' % (self.last_state_block, x, y, z)
        elif (op == '-'):
            stm= '%s.sub(%s,%s,%s);\n' % (self.last_state_block, x, y, z)
        elif (op == '*'):
            stm= '%s.mul(%s,%s,%s);\n' % (self.last_state_block, x, y, z)
        elif (op == '/'):
            stm= '%s.div(%s,%s,%s);\n' % (self.last_state_block, x, y, z)
        elif (op == '<<'):
            stm= '%s.shl(%s,%s,%s);\n' % (self.last_state_block, x, y, z)
        elif (op == '>>'):
            #since function lshr(a,b,0) does not work properly so lshr(a,b,0) will be converted to a = b
            if (int(z)==0):
                stm = '%s.assign(%s,%s);\n' % (self.last_state_block, x, y)
            else:
                stm= '%s.lshr(%s,%s,%s);\n' % (self.last_state_block, x, y, z)
        #self.check_bound(x)
        return  stm
    def arrayLoad (self, variable, arr, index):
        statements = '%s.array_load(%s, %s, %s, 1);\n' % (self.last_state_block, variable, arr, index)
        return statements

    def arrayStore (self, arr1, var, index1, index2, chk):
        statements = ''
        # var is a varible, a[i] = x
        if (not chk):
            statements = '%s.array_store(%s, %s, %s, 1);\n' % (self.last_state_block,arr1, index1, var)
        else:  # var is an array: a[i] = b[i] (var)
            statements = '%s.array_load(tmp_%s, %s, %s, 1);\n' % (self.last_state_block,var, var, index2)
            statements += '%s.array_store(%s, %s, tmp_%s, 1);\n' % (self.last_state_block,arr1, index1, var)
        return statements

    tmpIndex = 0
    if_level = 0
    else_level = 0
    if_nb = 0

    # to get the negative form of a condition in If
    def get_negative_Form(self,cond):
        negative_form = cond
        if negative_form.find('>=') > 0:  negative_form =negative_form.replace('>=', '<',1)
        elif negative_form.find('<=') > 0: negative_form =negative_form.replace('<=', '>',1)
        elif negative_form.find('==') > 0: negative_form =negative_form.replace('==', '!=',1)
        elif negative_form.find('!=') > 0: negative_form =negative_form.replace('!=', '==',1)
        elif negative_form.find('>'): negative_form = negative_form.replace('>', '<=',1)
        elif negative_form.find('<') > 0: negative_form =negative_form.replace('<', '>=',1)
        return negative_form


    def encodeAssignment (self, lval_str, statement_str):
        statements = ''
        binaryOp = ''
        # encode every atomic binaryOp, e.g (x+y), (x*y)
        while statement_str.find(')') > 0:
            p2 = statement_str.find(')')
            p1 = statement_str[:p2].rfind('(')
            binaryOp = statement_str[p1:p2 + 1]
            p3, p4 = binaryOp.find(' '), binaryOp.rfind(' ')
            op, y, z = binaryOp[p3 + 1:p4], binaryOp[1:p3], binaryOp[p4:len(binaryOp) - 1]
            # if y and z are not  array elements
            if ('[' not in (y + z)):
                pass
            else:  # in the case the statement contains arrays
                if ('[' in y):
                    index = y[y.find('[') + 1:y.find(']')]
                    self.tmpIndex = self.tmpIndex + 1
                    tmp = 'tmp%s' % self.tmpIndex
                    self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (tmp, tmp)
                    statements += self.arrayLoad(tmp, y[:y.find('[')], index)
                    y = tmp
                if ('[' in z):
                    index = z[z.find('[') + 1: z.find(']')]
                    self.tmpIndex = self.tmpIndex + 1
                    tmp = 'tmp%s' % self.tmpIndex
                    self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (tmp, tmp)
                    statements += self.arrayLoad(tmp, z[:z.find('[')], index)
                    z = tmp

            # we now replace a binaryOp by a tmp variable
            self.tmpIndex = self.tmpIndex + 1
            x = 'tmp%s' % self.tmpIndex
            self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (x, x)
            statement_str = statement_str[0:p1] + 'tmp%s' % self.tmpIndex + statement_str[p2 + 1:]
            statements += self.encode_basicAssignment(x, y, z, op)
        # encode the last binayOp (i.e x = y + tmp )
        if ('[' in lval_str):
            index = index = lval_str[lval_str.find('[') + 1:lval_str.find(']')]
            statements += 'block_%s_%s_%s.array_store(%s, %s, %s, 1);\n' % (
            self.if_nb, self.if_level, self.else_level, lval_str[:lval_str.find('[')], index, statement_str)
        else:
            statements += 'block_%s_%s_%s.assign(%s,%s);\n' % (
            self.if_nb, self.if_level, self.else_level, lval_str, statement_str)
        return statements


    # replace an array element by a variable, it is used to encode a binary op containing arr elements. eg a[i] = b[i] + 5 ===> { tmp= b[i]; a[i] = tmp + 5;}
    def load_arr_element(self, arr,index):
        self.tmpIndex += 1
        new_tmp = 'tmp%s'%self.tmpIndex
        self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (new_tmp, new_tmp)
        self.enc += '%s.array_load(%s, %s, %s, 1);\n' % (self.last_state_block, new_tmp, arr,index)
        smt = new_tmp
        return  smt

    def encode_UnaryOp(self,U):
        stm = ''
        if isinstance(U.expr, pycparser.c_ast.Constant):
            stm = U.op + U.expr.value
        elif isinstance(U.expr, pycparser.c_ast.ID):
            self.tmpIndex += 1
            new_tmp = 'tmp%s' % self.tmpIndex
            self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (new_tmp, new_tmp)
            self.enc += '%s.mul( %s, %s, -1);\n' % (self.last_state_block, new_tmp,  U.expr.name)
            stm = new_tmp
        elif isinstance(U.expr, pycparser.c_ast.ArrayRef):
            stm = self.load_arr_element(U.expr.name.name, U.expr.subscript.value)
            self.enc += '%s.mul( %s, %s, -1);\n' % (self.last_state_block, stm, stm)
        else:
            print 'this case has not been handled yet'
        return  stm;


    def typecasting(self, x, bit_wide):
        bound =''
        for key in self.bit_wide_dict:
            if int(self.bit_wide_dict[key]) == int(bit_wide):
                bound = self.bounds_dict[key]
        if (bound==''):

            bounds = 0
            bit_wide = int(bit_wide)
            if (bit_wide <= self.max_wide):
                bound = pow(2, bit_wide)
                bound = bound - 1
                self.enc += '%s.bitwise_and(%s,%s,%s);\n' % (self.last_state_block, x, x, bound)

            else:
                self.tmpIndex += 1
                new_tmp = 'tmp_%s'%self.tmpIndex
                self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (
                new_tmp, new_tmp)

                nbloop = bit_wide - self.max_wide;
                maxint = pow(2, self.max_wide)
                self.variable_declareration += 'entry.assign(%s,%s);\n' % (new_tmp, maxint)
                self.variable_declareration += 'for (int i =0; i < %s; i++) entry.mul(%s,%s,2);\n' % (
                    nbloop, new_tmp, new_tmp)
                self.variable_declareration += 'entry.sub(%s,%s,1);\n' % (new_tmp, new_tmp)
                self.enc += '%s.bitwise_and(%s,%s,%s);\n' % (self.last_state_block, x, x, new_tmp)
        else:
            self.enc += '%s.bitwise_and(%s,%s,%s);\n' % (self.last_state_block, x, x, bound)

    #encode assignment where the right side is a typecasting, e.g, a = (bitvector[10]) -b;

    def encode_Assign0 (self,n):
        stm = self.visit(n)

        left_side = ''
        bit_wide = str(int(stm[stm.find('[') + 1:stm.find(']')]) - 1)
        if isinstance(n.expr, pycparser.c_ast.UnaryOp):
            # print self.encode_UnaryOp(n.rvalue.left.expr)
            left_side = self.encode_UnaryOp(n.expr)
            self.typecasting(left_side, bit_wide)
        elif isinstance(n.expr, pycparser.c_ast.Constant):
            left_side = n.expr
        elif isinstance(n.expr, pycparser.c_ast.ID):
            left_side = n.expr.name
            self.typecasting(left_side, bit_wide)
        else:
            self.enc += '%s.array_load(tmp_%s, %s, %s, 1);\n' % (
                self.last_state_block, n.expr.name.name, n.expr.name.name,
                n.expr.subscript.value)
            left_side = 'tmp_%s' % n.expr.name.name
            self.typecasting(left_side, bit_wide)
        return  left_side
    #encode assignment where the right side is a binary op
    def encode_Assign1(self, n):
        if isinstance(n.rvalue.right, pycparser.c_ast.ArrayRef):
            right_side = self.load_arr_element(n.rvalue.right.name.name, n.rvalue.right.subscript.value)
        else:
            if isinstance(n.rvalue.right,pycparser.c_ast.Constant):
                right_side = n.rvalue.right.value
            elif isinstance(n.rvalue.right,pycparser.c_ast.UnaryOp):
                #print 'goi function Unary', n.rvalue.right.expr, n.rvalue.right.op
                right_side=self.encode_UnaryOp(n.rvalue.right)
            else:
                right_side = n.rvalue.right.name
        left_side=''
        if isinstance(n.rvalue.left, pycparser.c_ast.ArrayRef):
            left_side = self.load_arr_element(n.rvalue.left.name.name, n.rvalue.left.subscript.value)
        else:
            if isinstance(n.rvalue.left,pycparser.c_ast.Constant):
                left_side = n.rvalue.left.value
            elif isinstance(n.rvalue.left,pycparser.c_ast.UnaryOp):
                left_side =  self.encode_UnaryOp(n.rvalue.left)
            elif isinstance(n.rvalue.left,pycparser.c_ast.Cast):
               left_side= self.encode_Assign0(n.rvalue.left)
            else:
                left_side=  n.rvalue.left.name
        # now result = right_side op left_side
        if isinstance(n.lvalue,pycparser.c_ast.ID):
            self.enc += self.encode_basicAssignment(n.lvalue.name,left_side,right_side,n.rvalue.op)
        else:
            tmp= 'tmp_%s'%n.lvalue.name.name
            self.enc += self.encode_basicAssignment(tmp,left_side,right_side,n.rvalue.op)
            self.enc += '%s.array_store(%s, %s, %s, 1);\n' % (self.last_state_block, n.lvalue.name.name, n.lvalue.subscript.value, tmp)
    dem = 0
    def visit_Assignment (self, n):
        self.dem += 1
        rval_str = self._parenthesize_if(n.rvalue, lambda n: isinstance(n, pycparser.c_ast.Assignment))
        lval_str = self.visit(n.lvalue)
        assgmt = '%s %s (%s);' % (self.visit(n.lvalue), n.op, rval_str)
        '''
        if self.if_level == 0 and self.else_level == 0 :
            # create a new block and compute the parents for such block
            #new_block = 'block_%s_%s_%s' % (self.if_nb, self.if_level, self.else_level)
            #new_block_decl = 'basic_block_t& %s = prog.insert("%s");\n' % (new_block, new_block)
            # compute the parent for exit block
            #self.exit_parents = new_block + ' >> exit;\n'

            if len(self.last_ifelse_blocks) > 0:
                self.block_declareration += new_block_decl
                for block in self.last_ifelse_blocks:
                    #self.block_declareration += '%s >> %s;\n' % (block, new_block)
                    self.last_ifelse_blocks = []
                    self.last_state_block = new_block

            elif ( len(self.last_state_block ) > 1 and self.last_state_block != new_block):
                self.block_declareration += new_block_decl
                #self.block_declareration += '%s >> %s;\n'%(self.last_state_block,new_block)
                self.last_state_block = new_block
        '''
        # We now start to encode the assignment, basic case: e.g, x = y, x = a[i], x = 2, a[i] = x; a[i] = b[i] ...
        if (not isinstance(n.rvalue, pycparser.c_ast.BinaryOp) and not isinstance(n.rvalue, pycparser.c_ast.TernaryOp) and not isinstance(n.rvalue, pycparser.c_ast.Cast) ):
            if (not isinstance(n.rvalue,  pycparser.c_ast.ArrayRef)):  # the left side of the assignment is a variable or constant
                if (isinstance(n.rvalue, pycparser.c_ast.Constant)):
                    rvalue = n.rvalue.value
                elif (isinstance(n.rvalue, pycparser.c_ast.UnaryOp)):
                    #ss = self.visit(n.rvalue)
                    #print 'assignment === ', n.lvalue.name, ss
                    if (isinstance(n.rvalue.expr, pycparser.c_ast.ArrayRef)):
                        variable = 'tmp_for_%s'%n.rvalue.expr.name.name
                        stm = self.arrayLoad(variable, n.rvalue.expr.name.name, n.rvalue.expr.subscript.value)
                        stm = stm + '%s.mul(%s,%s,-1);\n'%(self.last_state_block,variable,variable);
                        self.enc += stm
                        rvalue = variable
                        #print  '+++++++++++++++++++++++++++++++++++++++++', n.rvalue.expr.name.name, n.rvalue.expr.subscript.value
                        #print 'tao ra mot cai ',stm
                    else:
                        rvalue = n.rvalue.op + n.rvalue.expr.value
                else:
                    rvalue = n.rvalue.name
                if (not '[' in lval_str):
                    self.enc = self.enc + 'block_%s_%s_%s.assign(%s,%s);\n' % (self.if_nb, self.if_level, self.else_level,n.lvalue.name, rvalue)
                else:  # a[i] = x  arr1,var,index1,index2,chk
                    self.enc = self.enc + self.arrayStore(n.lvalue.name.name, rvalue, n.lvalue.subscript.value, 0, 0)
            else:  # if the right side of the assignment is an array element
                if (not '[' in lval_str):
                    self.enc = self.enc + '%s.array_load(%s, %s, %s, 1);\n' % (self.last_state_block,
                    n.lvalue.name, n.rvalue.name.name, n.rvalue.subscript.value)
                else:  # (self, arr1,var,index1,index2,chk):
                    self.enc = self.enc + self.arrayStore(n.lvalue.name.name, n.rvalue.name.name,
                                                          n.lvalue.subscript.value, n.rvalue.subscript.value, 1)
        # we handle ternary Op here, e.g: a = (b > c)? x : y ;
        elif(isinstance(n.rvalue, pycparser.c_ast.TernaryOp)):
            self.if_nb +=1
            cond = self.visit(n.rvalue.cond)
            n_cond = self.get_negative_Form(cond)
            fbl = 'ter_block_%s_f'%self.ter_block
            tbl = 'ter_block_%s_t'%self.ter_block

            # Here handling Unary Op, for now only handle a simple case: a = (cond)?x : -y  ; not ... -2*y;
            if (isinstance(n.rvalue.iftrue,pycparser.c_ast.UnaryOp)):
                 if (n.rvalue.iftrue.op == '-'):
                    bl1 = '%s.assume(%s);\n' % (tbl, cond) + '%s.mul(%s,%s,-1); \n' % (tbl, n.lvalue.name, n.rvalue.iftrue.expr.name)
                 else:
                    print 'this case has not been handled'
            else:
                bl1 = '%s.assume(%s);\n' % (tbl, cond) + '%s.assign(%s,%s); \n' %(tbl, n.lvalue.name, n.rvalue.iftrue.name)
            if (isinstance( n.rvalue.iffalse,pycparser.c_ast.UnaryOp)):
               if (n.rvalue.iffalse.op == '-'):
                  bl2 = '%s.assume(%s); \n' % (fbl, n_cond) + '%s.mul(%s,%s,-1); \n' % (fbl, n.lvalue.name, n.rvalue.iffalse.expr.name)
            else:
                bl2 = '%s.assume(%s); \n' % (fbl, n_cond) + '%s.assign(%s,%s); \n' % ( fbl, n.lvalue.name, n.rvalue.iffalse.name)

            self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (tbl, tbl)
            self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (fbl, fbl)
            self.block_declareration += '%s >> %s;\n' % (self.last_state_block, fbl)
            self.block_declareration += '%s >> %s;\n' % (self.last_state_block, tbl)
            self.if_level = self.if_level + 1;
            new_block = 'block_%s_%s_%s' % (self.if_nb, self.if_level, self.else_level)
            self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (new_block, new_block)

            self.block_declareration += '%s >> %s;\n' % (tbl,new_block)
            self.block_declareration += '%s >> %s;\n' % (fbl,new_block)
            self.last_state_block = new_block
            self.enc = self.enc  + bl1 + bl2
            self.ter_block = self.ter_block +1
        # handling the right side of the assignment is a binary op:
        elif isinstance(n.rvalue, pycparser.c_ast.BinaryOp):
            self.encode_Assign1(n);
        # if  a = typecasting b;
        elif isinstance(n.rvalue, pycparser.c_ast.Cast):
            rside = self.encode_Assign0(n.rvalue)
            if isinstance(n.lvalue, pycparser.c_ast.ID):
                self.enc += '%s.assign(%s,%s);\n' % (self.last_state_block,n.lvalue.name,rside)
            else:
                self.enc += '%s.array_store(%s,%s,%s,1);\n' % (self.last_state_block,n.lvalue.name.name,n.lvalue.subscript.value,rside)
        else:
            left_val = self.visit(n.lvalue);
            self.enc = self.enc + self.encodeAssignment(left_val, '(' + rval_str + ')')

        # check bound for any assignment.
        # if the left side is a scala variable
        if isinstance(n.lvalue, pycparser.c_ast.ID):
            self.scala_assignt_bound(n.lvalue.name)
        else:
        # if the left side is an element of an array
            #print '------------------------------------------',n.lvalue.name.name,  n.lvalue.subscript.value
            arr = n.lvalue.name.name
            self.arr_assignt_bound(arr,n.lvalue.subscript.value)

        #if assignment is decided by a if, e.g (a > b) c = d, then update relationships between block.
        if (self.if_level > 0):
            new_block = 'block_%s_%s_%s' % (self.if_nb, self.if_level, self.else_level)
            decl = 'basic_block_t& entry = prog.insert("%s");\n'%new_block
            self.block_declareration += decl

            self.last_state_block = new_block
            self.block_declareration += '%s >> %s;\n' % (self.last_if_false_block, new_block)
            self.block_declareration += '%s >> %s;\n' % (self.last_if_true_block, new_block)
        return ''


    # to nomarlize a condition if it contains array elements
    def nomarlize_Cond(self, cond):
        statements = ''
        new_cond = cond

        for arr in self.arrays:
            #print arr
            s = arr+'['
            p = new_cond.find(s)
            while p >= 0:
                index =  index = new_cond[p+len(arr)+1:]
                index = index[:index.find(']')]
                self.tmpIndex = self.tmpIndex + 1
                tmp = 'tmp%s' % self.tmpIndex
                self.variable_declareration += 'var_t %s(vfac["%s"], crab::INT_TYPE, 32);\n' % (tmp, tmp)
                self.enc += self.arrayLoad(tmp, arr, index)
                new_cond = new_cond.replace(arr+'['+index + ']',tmp)
                #print new_cond
                p = new_cond.find(arr + '[')
        return  new_cond


    def visit_If1 (self, n):
        self.if_level += 1
        self.if_nb += 1
        s = 'if ('
        if n.cond: s += self.visit(n.cond)
        s += ')\n'
        true_block = 'if_%s_%s_%s_t' % (self.if_nb, self.if_level, self.else_level)
        false_block = 'if_%s_%s_%s_f' % (self.if_nb, self.if_level, self.else_level)

        #new_parents = [true_block]

        self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (true_block, true_block)
        self.block_declareration += 'basic_block_t& %s = prog.insert("%s");\n' % (false_block, false_block)
        self.block_declareration += '%s >> %s;\n' % (self.last_state_block, true_block)
        self.block_declareration += '%s >> %s;\n' % (self.last_state_block, false_block)
        self.last_if_true_block = true_block
        self.last_if_false_block = false_block
        self.last_state_block = true_block
        nomarlized_cond = self.nomarlize_Cond( self.visit(n.cond))

        '''
        # add parents for new_block
        parents = self.last_ifelse_blocks + [self.last_state_block]

        for block in parents :
            if(len(block) > 1):
                self.block_declareration += '%s >> %s;\n' % (block, new_block)
        '''
        self.enc += '%s.assume(%s); \n' % (true_block, nomarlized_cond)
        self.enc += '%s.assume(%s); \n' % (false_block, self.get_negative_Form(nomarlized_cond))

        s += self._generate_stmt(n.iftrue, add_indent=True)
        self.if_level -= 1

        if n.iffalse:
            print 'the solution for else is not complete yet'

        return s
        # return super(self.__class__, self).visit_If(n)

    def visit_If (self, n):
        nb_if = self.if_nb
        self.visit_If1(n)
        return ''

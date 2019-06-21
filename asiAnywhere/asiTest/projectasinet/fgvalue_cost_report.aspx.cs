
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class fgvalue_cost_report_list : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fgvalue_cost_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {
                CheckBox6.Visible = false;

                if (!Page.IsPostBack)
                {
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        TextBox1.Text = aDefaultCust;
                        TextBox2.Text = aDefaultCust;
                    }
                    catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'fgvalue_cost_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                           
                            TextBox7.Text = dr["field3"].ToString();
                            TextBox8.Text = dr["field4"].ToString();
                            BeBinTextBox.Text = dr["field5"].ToString();
                            EndBinTextBox.Text = dr["field6"].ToString();
                            TextBox3.Text = dr["field7"].ToString();
                            TextBox4.Text = dr["field8"].ToString();
                            BeCatTextBox.Text = dr["field9"].ToString();
                            EndCatTextBox.Text = dr["field10"].ToString();


                            if (dr["rd_field1"].ToString() == "Customer#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "FG Item#")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Part#")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "Product Category")
                                RadioButtonList1.SelectedIndex = 3;
                            if (dr["rd_field1"].ToString() == "Whs/Bin")
                                RadioButtonList1.SelectedIndex = 4;

                            if (dr["rd_field2"].ToString() == "Stock")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "Custom")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "All")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "Qty")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "MSF")
                                RadioButtonList3.SelectedIndex = 1;

                            if (dr["rd_field4"].ToString() == "FG")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "Order")
                                RadioButtonList4.SelectedIndex = 1;

                            if (dr["rd_field5"].ToString() == "Line")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "Header")
                                RadioButtonList5.SelectedIndex = 1;



                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;

                            if (dr["chk_field6"].ToString() == "Yes")
                                CheckBox6.Checked = true;
                            else
                                CheckBox6.Checked = false;

                            if (dr["chk_field7"].ToString() == "Yes")
                                CheckBox7.Checked = true;
                            else
                                CheckBox7.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                CheckBox8.Checked = true;
                            else
                                CheckBox8.Checked = false;
                            if (dr["chk_field9"].ToString() == "Yes")
                                CheckBox_rec_date.Checked = true;
                            else
                                CheckBox_rec_date.Checked = false;

                            if (dr["chk_field10"].ToString() == "Yes")
                                CheckBox_comp_only.Checked = true;
                            else
                                CheckBox_comp_only.Checked = false;

                            if (dr["chk_field11"].ToString() == "Yes")
                                CheckBox_sub_tot.Checked = true;
                            else
                                CheckBox_sub_tot.Checked = false;

                            if (dr["chk_field12"].ToString() == "Yes")
                                CheckBox_act_rel_qty.Checked = true;
                            else
                                CheckBox_act_rel_qty.Checked = false;

                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }
                    
                    TextBox2.ReadOnly = true;
                    Image2.Visible = false;
                    if(TextBox4.Text == "")
                    TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if(TextBox8.Text == "")                 
                    TextBox8.Text = "zzzzz";
                    if(EndBinTextBox.Text == "")
                    EndBinTextBox.Text = "zzzzzzzz";
                    if(EndCatTextBox.Text == "")
                    EndCatTextBox.Text = "zzzzzzzz";
                    
                    DateTextBox.Text = DateTime.Now.ToShortDateString();

                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        lblUser.Text = UserLogin.UserName;

                    }


                }

            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                    //try
                    //{
                    //    Label begin = (Label)FormView2.FindControl("CustLabel");
                    //    TextBox1.Text = begin.Text;
                    //    TextBox2.Text = "zzzzzzzz";
                    //}
                    //catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'fgvalue_cost_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            TextBox7.Text = dr["field3"].ToString();
                            TextBox8.Text = dr["field4"].ToString();
                            BeBinTextBox.Text = dr["field5"].ToString();
                            EndBinTextBox.Text = dr["field6"].ToString();
                            TextBox3.Text = dr["field7"].ToString();
                            TextBox4.Text = dr["field8"].ToString();
                            BeCatTextBox.Text = dr["field9"].ToString();
                            EndCatTextBox.Text = dr["field10"].ToString();


                            if (dr["rd_field1"].ToString() == "Customer#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "FG Item#")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Part#")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "Product Category")
                                RadioButtonList1.SelectedIndex = 3;
                            if (dr["rd_field1"].ToString() == "Whs/Bin")
                                RadioButtonList1.SelectedIndex = 4;

                            if (dr["rd_field2"].ToString() == "Stock")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "Custom")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "All")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "Qty")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "MSF")
                                RadioButtonList3.SelectedIndex = 1;

                            if (dr["rd_field4"].ToString() == "FG")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "Order")
                                RadioButtonList4.SelectedIndex = 1;

                            if (dr["rd_field5"].ToString() == "Line")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "Header")
                                RadioButtonList5.SelectedIndex = 1;



                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;

                            if (dr["chk_field6"].ToString() == "Yes")
                                CheckBox6.Checked = true;
                            else
                                CheckBox6.Checked = false;

                            if (dr["chk_field7"].ToString() == "Yes")
                                CheckBox7.Checked = true;
                            else
                                CheckBox7.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                CheckBox8.Checked = true;
                            else
                                CheckBox8.Checked = false;

                            if (dr["chk_field9"].ToString() == "Yes")
                                CheckBox_rec_date.Checked = true;
                            else
                                CheckBox_rec_date.Checked = false;

                            if (dr["chk_field10"].ToString() == "Yes")
                                CheckBox_comp_only.Checked = true;
                            else
                                CheckBox_comp_only.Checked = false;

                            if (dr["chk_field11"].ToString() == "Yes")
                                CheckBox_sub_tot.Checked = true;
                            else
                                CheckBox_sub_tot.Checked = false;

                            if (dr["chk_field12"].ToString() == "Yes")
                                CheckBox_act_rel_qty.Checked = true;
                            else
                                CheckBox_act_rel_qty.Checked = false;

                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }

                    if (TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if (TextBox8.Text == "")
                        TextBox8.Text = "zzzzz";
                    if (EndBinTextBox.Text == "")
                        EndBinTextBox.Text = "zzzzzzzz";
                    if (EndCatTextBox.Text == "")
                        EndCatTextBox.Text = "zzzzzzzz";

                    DateTextBox.Text = DateTime.Now.ToShortDateString();

                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        lblUser.Text = UserLogin.UserName;

                    }                    

                }

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }




        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

       


    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        if (CheckBox1.Checked)
            HiddenField6.Value = "Yes";
        else
            HiddenField6.Value = "No";

        if (CheckBox2.Checked)
            HiddenField7.Value = "Yes";
        else
            HiddenField7.Value = "No";

        if (CheckBox3.Checked)
            HiddenField8.Value = "Yes";
        else
            HiddenField8.Value = "No";

        if (CheckBox4.Checked)
            HiddenField9.Value = "Yes";
        else
            HiddenField9.Value = "No";

        if (CheckBox5.Checked)
            HiddenField10.Value = "Yes";
        else
            HiddenField10.Value = "No";

        if (CheckBox6.Checked)
            HiddenField11.Value = "Yes";
        else
            HiddenField11.Value = "No";

        if (CheckBox7.Checked)
            HiddenField12.Value = "Yes";
        else
            HiddenField12.Value = "No";

        if (CheckBox8.Checked)
            HiddenField13.Value = "Yes";
        else
            HiddenField13.Value = "No";

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "Customer#";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "FG Item#";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField1.Value = "Part#";
        if (RadioButtonList1.SelectedIndex == 3)
            HiddenField1.Value = "Product Category";
        if (RadioButtonList1.SelectedIndex == 4)
            HiddenField1.Value = "Whs/Bin";


        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField2.Value = "Stock";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField2.Value = "Custom";
        if (RadioButtonList2.SelectedIndex == 2)
            HiddenField2.Value = "All";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField3.Value = "Qty";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField3.Value = "MSF";

        if (RadioButtonList4.SelectedIndex == 0)
            HiddenField4.Value = "FG";
        if (RadioButtonList4.SelectedIndex == 1)
            HiddenField4.Value = "Order";

        if (RadioButtonList5.SelectedIndex == 0)
            HiddenField5.Value = "Line";
        if (RadioButtonList5.SelectedIndex == 1)
            HiddenField5.Value = "Header";

        if (CheckBox_rec_date.Checked)
            HiddenField14.Value = "Yes";
        else
            HiddenField14.Value = "No";

        if (CheckBox_comp_only.Checked)
            HiddenField15.Value = "Yes";
        else
            HiddenField15.Value = "No";

        if (CheckBox_sub_tot.Checked)
            HiddenField16.Value = "Yes";
        else
            HiddenField16.Value = "No";

        if (CheckBox_act_rel_qty.Checked)
            HiddenField17.Value = "Yes";
        else
            HiddenField17.Value = "No";


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fgvalue_cost_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim();
            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
            }
        }

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = lblUser.Text;
        ObjectDataSource1.SelectParameters["prmFgcost"].DefaultValue = "fgcost";
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = DateTextBox.Text.Trim(); ;
        // ObjectDataSource1.SelectParameters["prmDaysOld"].DefaultValue = TextBox10.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmBeginWhse"].DefaultValue = TextBox7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndWhse"].DefaultValue = TextBox8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginLocBin"].DefaultValue = BeBinTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmEndLocBin"].DefaultValue = EndBinTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginCat"].DefaultValue = BeCatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCat"].DefaultValue = EndCatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField1.Value; ;
        ObjectDataSource1.SelectParameters["prmIcode"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField3.Value;

        ObjectDataSource1.SelectParameters["prmFrom"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmSellPrice"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmIncludeZero"].DefaultValue = HiddenField7.Value;

        ObjectDataSource1.SelectParameters["prmIncludeCustWhse"].DefaultValue = HiddenField9.Value; ;
        ObjectDataSource1.SelectParameters["prmIncludeOnlyCustWhse"].DefaultValue = HiddenField13.Value;
        ObjectDataSource1.SelectParameters["prmPrintCost"].DefaultValue = HiddenField11.Value;
        ObjectDataSource1.SelectParameters["prmDlMat"].DefaultValue = HiddenField12.Value; ;
        ObjectDataSource1.SelectParameters["prmPrintCustPart"].DefaultValue = HiddenField8.Value;
        ObjectDataSource1.SelectParameters["prmPrintPo"].DefaultValue = HiddenField10.Value; ;
        ObjectDataSource1.SelectParameters["prmPoType"].DefaultValue = HiddenField5.Value;

        ObjectDataSource1.SelectParameters["prmPrintDate"].DefaultValue = HiddenField14.Value; ;
        ObjectDataSource1.SelectParameters["prmPrintCompOnly"].DefaultValue = HiddenField15.Value;
        ObjectDataSource1.SelectParameters["prmPrintSubTot"].DefaultValue = HiddenField16.Value; ;
        ObjectDataSource1.SelectParameters["prmPrintActRelQty"].DefaultValue = HiddenField17.Value;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'fgvalue_cost_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9,field10, rd_field1,rd_field2,rd_field3,rd_field4,rd_field5, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6,chk_field7,chk_field8,chk_field9, chk_field10,chk_field11,chk_field12) values ('" + UserLogin.UserName + "','fgvalue_cost_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + BeBinTextBox.Text.Trim() + "','" + EndBinTextBox.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + BeCatTextBox.Text.Trim() + "','" + EndCatTextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "','" + HiddenField12.Value + "','" + HiddenField13.Value + "','" + HiddenField14.Value + "','" + HiddenField15.Value + "','" + HiddenField16.Value + "','" + HiddenField17.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox7.Text.Trim() + "', field4 = '" + TextBox8.Text.Trim() + "', field5 = '" + BeBinTextBox.Text.Trim() + "', field6 = '" + EndBinTextBox.Text.Trim() + "', field7 = '" + TextBox3.Text.Trim() + "', field8 = '" + TextBox4.Text.Trim() + "', field9 = '" + BeCatTextBox.Text.Trim() + "', field10 = '" + EndCatTextBox.Text.Trim() + "', rd_field1 = '" + HiddenField1.Value + "', rd_field2 = '" + HiddenField2.Value + "', rd_field3 = '" + HiddenField3.Value + "', rd_field4 = '" + HiddenField4.Value + "', rd_field5 = '" + HiddenField5.Value + "', chk_field1 = '" + HiddenField6.Value + "', chk_field2 = '" + HiddenField7.Value + "', chk_field3 = '" + HiddenField8.Value + "', chk_field4 = '" + HiddenField9.Value + "', chk_field5 = '" + HiddenField10.Value + "', chk_field6 = '" + HiddenField11.Value + "', chk_field7 = '" + HiddenField12.Value + "', chk_field8 = '" + HiddenField13.Value + "', chk_field9 = '" + HiddenField14.Value + "', chk_field10 = '" + HiddenField15.Value + "', chk_field11 = '" + HiddenField16.Value + "', chk_field12 = '" + HiddenField17.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='fgvalue_cost_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }



        try
        {
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("vfgvalueLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='fgvalue_cost_report.aspx'</script>");
            }
        }
        catch
        {

        }


    }



}

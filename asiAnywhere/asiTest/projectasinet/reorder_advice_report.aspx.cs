
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

public partial class reorder_advice_report_list : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "reorder_advice_report.aspx";
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
                if (!Page.IsPostBack)
                {
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

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'reorder_advice_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                            
                            TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            TextBox5.Text = dr["field5"].ToString();
                            TextBox6.Text = dr["field6"].ToString();
                            TextBox7.Text = dr["field7"].ToString();
                            TextBox8.Text = dr["field8"].ToString();
                            TextBox9.Text = dr["field9"].ToString();


                            if (dr["rd_field1"].ToString() == "Total Allocated")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Scheduled Releases")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Actual Releases")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "All Releases")
                                RadioButtonList1.SelectedIndex = 3;

                            if (dr["rd_field2"].ToString() == "Stocked")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "Not Stocked")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "All")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "Purchased")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "Manufactured")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "All")
                                RadioButtonList3.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "Lot Controlled")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "Reorder")
                                RadioButtonList4.SelectedIndex = 1;
                            if (dr["rd_field4"].ToString() == "All")
                                RadioButtonList4.SelectedIndex = 2;

                            if (dr["rd_field5"].ToString() == "Qty Avail")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "Vendor")
                                RadioButtonList5.SelectedIndex = 1;

                            if (dr["rd_field6"].ToString() == "Price")
                                RadioButtonList6.SelectedIndex = 0;
                            if (dr["rd_field6"].ToString() == "Vendor")
                                RadioButtonList6.SelectedIndex = 1;
                            if (dr["rd_field6"].ToString() == "Max Qty")
                                RadioButtonList6.SelectedIndex = 2;


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

                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    Image3.Visible = false;
                    TextBox2.ReadOnly = true;


                    if (TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if (TextBox6.Text == "")
                        TextBox6.Text = "zzzzz";
                    if (TextBox8.Text == "")
                        TextBox8.Text = "zzzzz";
                    RadioButtonList8.SelectedIndex = 0;
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
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'reorder_advice_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            TextBox5.Text = dr["field5"].ToString();
                            TextBox6.Text = dr["field6"].ToString();
                            TextBox7.Text = dr["field7"].ToString();
                            TextBox8.Text = dr["field8"].ToString();
                            TextBox9.Text = dr["field9"].ToString();


                            if (dr["rd_field1"].ToString() == "Total Allocated")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Scheduled Releases")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Actual Releases")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "All Releases")
                                RadioButtonList1.SelectedIndex = 3;

                            if (dr["rd_field2"].ToString() == "Stocked")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "Not Stocked")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "All")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "Purchased")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "Manufactured")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "All")
                                RadioButtonList3.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "Lot Controlled")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "Reorder")
                                RadioButtonList4.SelectedIndex = 1;
                            if (dr["rd_field4"].ToString() == "All")
                                RadioButtonList4.SelectedIndex = 2;

                            if (dr["rd_field5"].ToString() == "Qty Avail")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "Vendor")
                                RadioButtonList5.SelectedIndex = 1;

                            if (dr["rd_field6"].ToString() == "Price")
                                RadioButtonList6.SelectedIndex = 0;
                            if (dr["rd_field6"].ToString() == "Vendor")
                                RadioButtonList6.SelectedIndex = 1;
                            if (dr["rd_field6"].ToString() == "Max Qty")
                                RadioButtonList6.SelectedIndex = 2;


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

                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;

                    if (TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if (TextBox6.Text == "")
                        TextBox6.Text = "zzzzz";
                    if (TextBox8.Text == "")
                        TextBox8.Text = "zzzzz";
                    RadioButtonList8.SelectedIndex = 0;
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
            HiddenField1.Value = "Yes";
        else
            HiddenField1.Value = "No";

        if (CheckBox2.Checked)
            HiddenField2.Value = "Yes";
        else
            HiddenField2.Value = "No";

        if (CheckBox3.Checked)
            HiddenField3.Value = "Yes";
        else
            HiddenField3.Value = "No";

        if (CheckBox4.Checked)
            HiddenField4.Value = "Yes";
        else
            HiddenField4.Value = "No";

        if (CheckBox5.Checked)
            HiddenField5.Value = "Yes";
        else
            HiddenField5.Value = "No";

        if (CheckBox6.Checked)
            HiddenField6.Value = "Yes";
        else
            HiddenField6.Value = "No";

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField7.Value = "Total Allocated";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField7.Value = "Scheduled Releases";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField7.Value = "Actual Releases";
        if (RadioButtonList1.SelectedIndex == 3)
            HiddenField7.Value = "All Releases";
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField8.Value = "No";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField8.Value = "Yes";

        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField9.Value = "Stocked";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField9.Value = "Not Stocked";
        if (RadioButtonList2.SelectedIndex == 2)
            HiddenField9.Value = "All";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField10.Value = "Purchased";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField10.Value = "Manufactured";
        if (RadioButtonList3.SelectedIndex == 2)
            HiddenField10.Value = "All";

        if (RadioButtonList4.SelectedIndex == 0)
            HiddenField11.Value = "Lot Controlled";
        if (RadioButtonList4.SelectedIndex == 1)
            HiddenField11.Value = "Reorder";
        if (RadioButtonList4.SelectedIndex == 2)
            HiddenField11.Value = "All";

        if (RadioButtonList5.SelectedIndex == 0)
            HiddenField12.Value = "Qty Avail";
        if (RadioButtonList5.SelectedIndex == 1)
            HiddenField12.Value = "Vendor";
        

        if (RadioButtonList6.SelectedIndex == 0)
            HiddenField13.Value = "Price";
        if (RadioButtonList6.SelectedIndex == 1)
            HiddenField13.Value = "Vendor";
        if (RadioButtonList6.SelectedIndex == 2)
            HiddenField13.Value = "Max Qty";


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "reorder_advice_report.aspx";
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
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Reorder";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField8.Value;
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
        
        ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginCategory"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCategory"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginWare"].DefaultValue = TextBox7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndWare"].DefaultValue = TextBox8.Text.Trim();

        ObjectDataSource1.SelectParameters["prmQtyOnHand"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prIncQoh"].DefaultValue = Convert.ToString(CheckBox3.Checked);
        ObjectDataSource1.SelectParameters["prmBelow"].DefaultValue = Convert.ToString(CheckBox5.Checked);
        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        ObjectDataSource1.SelectParameters["prmHistory"].DefaultValue = Convert.ToString(CheckBox4.Checked);
        ObjectDataSource1.SelectParameters["prmDash"].DefaultValue = Convert.ToString(CheckBox6.Checked);
        ObjectDataSource1.SelectParameters["prmRdqoh"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = TextBox9.Text.Trim();

        ObjectDataSource1.SelectParameters["prmPrintStock"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["prmPrintPurch"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["prmPrintLot"].DefaultValue = HiddenField11.Value; 
        ObjectDataSource1.SelectParameters["prmPrintQty"].DefaultValue = HiddenField12.Value;
        ObjectDataSource1.SelectParameters["prmPrintPrice"].DefaultValue = HiddenField13.Value;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'reorder_advice_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9, rd_field1,rd_field2,rd_field3,rd_field4,rd_field5,rd_field6, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6) values ('" + UserLogin.UserName + "','reorder_advice_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + TextBox9.Text.Trim() + "','" + HiddenField7.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "','" + HiddenField12.Value + "','" + HiddenField13.Value + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();                
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "', field7 = '" + TextBox7.Text.Trim() + "', field8 = '" + TextBox8.Text.Trim() + "', field9 = '" + TextBox9.Text.Trim() + "', rd_field1 = '" + HiddenField7.Value + "', rd_field2 = '" + HiddenField9.Value + "', rd_field3 = '" + HiddenField10.Value + "', rd_field4 = '" + HiddenField11.Value + "', rd_field5 = '" + HiddenField12.Value + "', rd_field6 = '" + HiddenField13.Value + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "', chk_field5 = '" + HiddenField5.Value + "', chk_field6 = '" + HiddenField6.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='reorder_advice_report.aspx' ", conn);
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



        if (RadioButtonList8.SelectedIndex == 1)
        {
            try
            {
                OutputLabel.Visible = true;
                HyperLink1.Visible = true;
                Label path = (Label)FormView1.FindControl("vFileLabel");
                HyperLink1.Text = path.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

                if (path.Text == "")
                {
                    Label1.Text = "No Csv Exists";
                    Response.Write("<script>window.location.href='reorder_advice_report.aspx'</script>");
                }
            }
            catch
            {

            }
        }
        if (RadioButtonList8.SelectedIndex == 0)
        {

            try
            {
                Label vpath = (Label)FormView1.FindControl("vFileLabel");

                if (vpath.Text != "")
                {
                    string path = vpath.Text;
                    string path2 = @"/pdfs/" + path;
                    Session["open_REorder_list"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>var win=window.open('print_reorder_list.aspx'); target='_blank'; if(win==null || win=='undefined') alert('Popup has Blocked this page. To open this page enable popups.');</script>");
                        else
                            Response.Redirect("reorder_advice_report.aspx");
                    }
                }
                else
                {
                    Label1.Text = "No Pdf Exists";
                }
            }
            catch
            {
                Label1.Text = "No Pdf Exists";
            }
            if (Label1.Text == "")
            {
                Response.Write("<script>window.location.href='reorder_advice_report.aspx'</script>");
            }
        }
    }
    protected void TextBox1_textchanged(object sender, EventArgs e)
    {
        if (TextBox1.Text != "")
            Session["reorder_begin_cust"] = TextBox1.Text;
        else
            Session["reorder_begin_cust"] = null;
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label vpath = (Label)FormView1.FindControl("vFileLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_reorder_list.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("reorder_advice_report.aspx");
                }
            }
            else
            {
                //Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            //Label1.Text = "No Pdf Exists";
        }
    }

}

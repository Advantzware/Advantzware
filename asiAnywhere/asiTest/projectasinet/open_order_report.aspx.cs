
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

public partial class open_order_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "open_order_report.aspx";
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

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'open_order_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                           
                            BeginorderTextBox.Text = dr["field3"].ToString();
                            endingorderTextBox.Text = dr["field4"].ToString();
                            becustpoTextBox.Text = dr["field5"].ToString();
                            endcustpoTextBox.Text = dr["field6"].ToString();
                            bejobTextBox.Text = dr["field7"].ToString();
                            endjobTextBox.Text = dr["field8"].ToString();
                            bejob2TextBox.Text = dr["field9"].ToString();
                            endjob2TextBox.Text = dr["field10"].ToString();
                            TextBox3.Text = dr["field11"].ToString();
                            TextBox4.Text = dr["field12"].ToString();

                            BeCADTextBox.Text = dr["field13"].ToString();
                            endCADTextBox.Text = dr["field14"].ToString();
                            beDueTextBox.Text = dr["field15"].ToString();
                            enddueTextBox.Text = dr["field16"].ToString();
                            beuserTextBox.Text = dr["field17"].ToString();
                            enduserTextBox.Text = dr["field18"].ToString();
                            besmanTextBox.Text = dr["field19"].ToString();
                            endsmanTextBox.Text = dr["field20"].ToString();


                            if (dr["rd_field1"].ToString() == "C")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "D")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "S")
                                RadioButtonList2.SelectedIndex = 2;
                           

                            if (dr["rd_field2"].ToString() == "P")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "I")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "C")
                                RadioButtonList3.SelectedIndex = 2;
                            if (dr["rd_field2"].ToString() == "F")
                                RadioButtonList3.SelectedIndex = 3;
                            if (dr["rd_field2"].ToString() == "O")
                                RadioButtonList3.SelectedIndex = 4;
                            if (dr["rd_field2"].ToString() == "D")
                                RadioButtonList3.SelectedIndex = 5;
                            if (dr["rd_field2"].ToString() == "CA")
                                RadioButtonList3.SelectedIndex = 6;

                            if (dr["rd_field3"].ToString() == "O")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "C")
                                RadioButtonList4.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "A")
                                RadioButtonList4.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "O")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "C")
                                RadioButtonList5.SelectedIndex = 1;
                            if (dr["rd_field4"].ToString() == "A")
                                RadioButtonList5.SelectedIndex = 2;

                            if (dr["rd_field5"].ToString() == "L")
                                RadioButtonList6.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "R")
                                RadioButtonList6.SelectedIndex = 1;
                          

                            if (dr["rd_field6"].ToString() == "O")
                                RadioButtonList7.SelectedIndex = 0;
                            if (dr["rd_field6"].ToString() == "J")
                                RadioButtonList7.SelectedIndex = 1;
                            

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
                    try
                    {
                        
                        TextBox2.ReadOnly = true;
                        Image12.Visible = false;
                    }
                    catch { }
                    if(TextBox4.Text == "")
                    TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if(endcustpoTextBox.Text == "")
                    endcustpoTextBox.Text = "zzzzzzzzzzzzzzz";
                    if(endjobTextBox.Text == "")
                    endjobTextBox.Text = "zzzzzzzz";
                    if(endCADTextBox.Text == "")
                    endCADTextBox.Text = "zzzzzzzzzzzzzzz";
                    if(enduserTextBox.Text == "")
                    enduserTextBox.Text = "zzzzzzzz";
                    if(endsmanTextBox.Text == "")
                    endsmanTextBox.Text = "zzz";
                    if( bejob2TextBox.Text == "")
                    bejob2TextBox.Text = "-00";
                    if(endjob2TextBox.Text == "")
                    endjob2TextBox.Text = "-99";
                    if(BeginorderTextBox.Text == "")
                    BeginorderTextBox.Text = "01/05/2005";
                    if(endingorderTextBox.Text == "")
                    endingorderTextBox.Text = "12/31/2099";
                    if(beDueTextBox.Text =="")
                    beDueTextBox.Text = "01/01/0001";
                    if(enddueTextBox.Text == "")
                    enddueTextBox.Text = "12/31/2099";
                RadioButtonList8.SelectedIndex = 0;
                    //TextBox8.Text = "zzzzz";
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

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'open_order_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            BeginorderTextBox.Text = dr["field3"].ToString();
                            endingorderTextBox.Text = dr["field4"].ToString();
                            becustpoTextBox.Text = dr["field5"].ToString();
                            endcustpoTextBox.Text = dr["field6"].ToString();
                            bejobTextBox.Text = dr["field7"].ToString();
                            endjobTextBox.Text = dr["field8"].ToString();
                            bejob2TextBox.Text = dr["field9"].ToString();
                            endjob2TextBox.Text = dr["field10"].ToString();
                            TextBox3.Text = dr["field11"].ToString();
                            TextBox4.Text = dr["field12"].ToString();

                            BeCADTextBox.Text = dr["field13"].ToString();
                            endCADTextBox.Text = dr["field14"].ToString();
                            beDueTextBox.Text = dr["field15"].ToString();
                            enddueTextBox.Text = dr["field16"].ToString();
                            beuserTextBox.Text = dr["field17"].ToString();
                            enduserTextBox.Text = dr["field18"].ToString();
                            besmanTextBox.Text = dr["field19"].ToString();
                            endsmanTextBox.Text = dr["field20"].ToString();


                            if (dr["rd_field1"].ToString() == "C")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "D")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "S")
                                RadioButtonList2.SelectedIndex = 2;


                            if (dr["rd_field2"].ToString() == "P")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "I")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "C")
                                RadioButtonList3.SelectedIndex = 2;
                            if (dr["rd_field2"].ToString() == "F")
                                RadioButtonList3.SelectedIndex = 3;
                            if (dr["rd_field2"].ToString() == "O")
                                RadioButtonList3.SelectedIndex = 4;
                            if (dr["rd_field2"].ToString() == "D")
                                RadioButtonList3.SelectedIndex = 5;
                            if (dr["rd_field2"].ToString() == "CA")
                                RadioButtonList3.SelectedIndex = 6;

                            if (dr["rd_field3"].ToString() == "O")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "C")
                                RadioButtonList4.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "A")
                                RadioButtonList4.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "O")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "C")
                                RadioButtonList5.SelectedIndex = 1;
                            if (dr["rd_field4"].ToString() == "A")
                                RadioButtonList5.SelectedIndex = 2;

                            if (dr["rd_field5"].ToString() == "L")
                                RadioButtonList6.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "R")
                                RadioButtonList6.SelectedIndex = 1;


                            if (dr["rd_field6"].ToString() == "O")
                                RadioButtonList7.SelectedIndex = 0;
                            if (dr["rd_field6"].ToString() == "J")
                                RadioButtonList7.SelectedIndex = 1;


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

                    if (TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if (endcustpoTextBox.Text == "")
                        endcustpoTextBox.Text = "zzzzzzzzzzzzzzz";
                    if (endjobTextBox.Text == "")
                        endjobTextBox.Text = "zzzzzzzz";
                    if (endCADTextBox.Text == "")
                        endCADTextBox.Text = "zzzzzzzzzzzzzzz";
                    if (enduserTextBox.Text == "")
                        enduserTextBox.Text = "zzzzzzzz";
                    if (endsmanTextBox.Text == "")
                        endsmanTextBox.Text = "zzz";
                    if (bejob2TextBox.Text == "")
                        bejob2TextBox.Text = "-00";
                    if (endjob2TextBox.Text == "")
                        endjob2TextBox.Text = "-99";
                    if (BeginorderTextBox.Text == "")
                        BeginorderTextBox.Text = "01/05/2005";
                    if (endingorderTextBox.Text == "")
                        endingorderTextBox.Text = "12/31/2099";
                    if (beDueTextBox.Text == "")
                        beDueTextBox.Text = "01/01/0001";
                    if (enddueTextBox.Text == "")
                        enddueTextBox.Text = "12/31/2099";
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
        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField1.Value = "C";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField1.Value = "D";
        if (RadioButtonList2.SelectedIndex == 2)
            HiddenField1.Value = "S";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField2.Value = "P";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField2.Value = "I";
        if (RadioButtonList3.SelectedIndex == 2)
            HiddenField2.Value = "C";
        if (RadioButtonList3.SelectedIndex == 3)
            HiddenField2.Value = "F";
        if (RadioButtonList3.SelectedIndex == 4)
            HiddenField2.Value = "O";
        if (RadioButtonList3.SelectedIndex == 5)
            HiddenField2.Value = "D";
        if (RadioButtonList3.SelectedIndex == 6)
            HiddenField2.Value = "CA";

        if (RadioButtonList4.SelectedIndex == 0)
            HiddenField3.Value = "O";
        if (RadioButtonList4.SelectedIndex == 1)
            HiddenField3.Value = "C";
        if (RadioButtonList4.SelectedIndex == 2)
            HiddenField3.Value = "A";

        if (RadioButtonList5.SelectedIndex == 0)
            HiddenField4.Value = "O";
        if (RadioButtonList5.SelectedIndex == 1)
            HiddenField4.Value = "C";
        if (RadioButtonList5.SelectedIndex == 2)
            HiddenField4.Value = "A";

        if (RadioButtonList6.SelectedIndex == 0)
            HiddenField5.Value = "L";
        if (RadioButtonList6.SelectedIndex == 1)
            HiddenField5.Value = "R";

        if (RadioButtonList7.SelectedIndex == 0)
            HiddenField6.Value = "O";
        if (RadioButtonList7.SelectedIndex == 1)
            HiddenField6.Value = "J";

        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField7.Value = "Yes";


        if (CheckBox1.Checked)
        {
            HiddenField8.Value = "Yes";
        }
        else
        {
            HiddenField8.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField9.Value = "Yes";
        }
        else
        {
            HiddenField9.Value = "No";
        }

        if (CheckBox3.Checked)
        {
            HiddenField10.Value = "Yes";
        }
        else
        {
            HiddenField10.Value = "No";
        }

        if (CheckBox4.Checked)
        {
            HiddenField11.Value = "Yes";
        }
        else
        {
            HiddenField11.Value = "No";
        }

        if (CheckBox5.Checked)
        {
            HiddenField12.Value = "Yes";
        }
        else
        {
            HiddenField12.Value = "No";
        }

        if (CheckBox6.Checked)
        {
            HiddenField13.Value = "Yes";
        }
        else
        {
            HiddenField13.Value = "No";
        }



        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "open_order_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);


            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim(); ;
                ObjectDataSource1.SelectParameters["prmBegOrdate"].DefaultValue = BeginorderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrdate"].DefaultValue = endingorderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegPo"].DefaultValue = becustpoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndPo"].DefaultValue = endcustpoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegJob"].DefaultValue = bejobTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob"].DefaultValue = endjobTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBegJob2"].DefaultValue = bejob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob2"].DefaultValue = endjob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegCad"].DefaultValue = BeCADTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCad"].DefaultValue = endCADTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegDue"].DefaultValue = beDueTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndDue"].DefaultValue = enddueTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBegUser"].DefaultValue = beuserTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndUser"].DefaultValue = enduserTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegSman"].DefaultValue = besmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = endsmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSortcust"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmSecondary"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmJobStat"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmOrdStat"].DefaultValue = HiddenField4.Value;

                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = HiddenField5.Value;
                ObjectDataSource1.SelectParameters["prmWipqty"].DefaultValue = HiddenField6.Value;
                ObjectDataSource1.SelectParameters["prmPrintjob"].DefaultValue = HiddenField9.Value;
                ObjectDataSource1.SelectParameters["prmDroporder"].DefaultValue = HiddenField11.Value;
                ObjectDataSource1.SelectParameters["prmInorder"].DefaultValue = HiddenField10.Value;
                ObjectDataSource1.SelectParameters["prmInqty"].DefaultValue = HiddenField8.Value;
                ObjectDataSource1.SelectParameters["prmInact"].DefaultValue = HiddenField12.Value;
                ObjectDataSource1.SelectParameters["prminjob"].DefaultValue = HiddenField13.Value;
                ObjectDataSource1.SelectParameters["prmOutexcel"].DefaultValue = HiddenField7.Value;

            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim(); ;
                ObjectDataSource1.SelectParameters["prmBegOrdate"].DefaultValue = BeginorderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrdate"].DefaultValue = endingorderTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegPo"].DefaultValue = becustpoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndPo"].DefaultValue = endcustpoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegJob"].DefaultValue = bejobTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob"].DefaultValue = endjobTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBegJob2"].DefaultValue = bejob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob2"].DefaultValue = endjob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegCad"].DefaultValue = BeCADTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCad"].DefaultValue = endCADTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegDue"].DefaultValue = beDueTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndDue"].DefaultValue = enddueTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBegUser"].DefaultValue = beuserTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndUser"].DefaultValue = enduserTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegSman"].DefaultValue = besmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = endsmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSortcust"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmSecondary"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmJobStat"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmOrdStat"].DefaultValue = HiddenField4.Value;

                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = HiddenField5.Value;
                ObjectDataSource1.SelectParameters["prmWipqty"].DefaultValue = HiddenField6.Value;
                ObjectDataSource1.SelectParameters["prmPrintjob"].DefaultValue = HiddenField9.Value;
                ObjectDataSource1.SelectParameters["prmDroporder"].DefaultValue = HiddenField11.Value;
                ObjectDataSource1.SelectParameters["prmInorder"].DefaultValue = HiddenField10.Value;
                ObjectDataSource1.SelectParameters["prmInqty"].DefaultValue = HiddenField8.Value;
                ObjectDataSource1.SelectParameters["prmInact"].DefaultValue = HiddenField12.Value;
                ObjectDataSource1.SelectParameters["prminjob"].DefaultValue = HiddenField13.Value;
                ObjectDataSource1.SelectParameters["prmOutexcel"].DefaultValue = HiddenField7.Value;
            }

        }


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'open_order_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15,field16, field17, field18, field19, field20, rd_field1, rd_field2, rd_field3, rd_field4, rd_field5, rd_field6, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6) values ('" + UserLogin.UserName + "','open_order_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + BeginorderTextBox.Text.Trim() + "','" + endingorderTextBox.Text.Trim() + "','" + becustpoTextBox.Text.Trim() + "','" + endcustpoTextBox.Text.Trim() + "','" + bejobTextBox.Text.Trim() + "','" + endjobTextBox.Text.Trim() + "','" + bejob2TextBox.Text.Trim() + "','" + endjob2TextBox.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + BeCADTextBox.Text.Trim() + "','" + endCADTextBox.Text.Trim() + "','" + beDueTextBox.Text.Trim() + "','" + enddueTextBox.Text.Trim() + "','" + beuserTextBox.Text.Trim() + "','" + enduserTextBox.Text.Trim() + "','" + besmanTextBox.Text.Trim() + "','" + endsmanTextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "','" + HiddenField12.Value + "','" + HiddenField13.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox1.Text.Trim() + "', field3 = '" + BeginorderTextBox.Text.Trim() + "', field4 = '" + endingorderTextBox.Text.Trim() + "', field5 = '" + becustpoTextBox.Text.Trim() + "', field6 = '" + endcustpoTextBox.Text.Trim() + "', field7 = '" + bejobTextBox.Text.Trim() + "', field8 = '" + endjobTextBox.Text.Trim() + "', field9 = '" + bejob2TextBox.Text.Trim() + "', field10 = '" + endjob2TextBox.Text.Trim() + "', field11 = '" + TextBox3.Text.Trim() + "', field12 = '" + TextBox4.Text.Trim() + "', field13 =  '" + BeCADTextBox.Text.Trim() + "', field14 = '" + endCADTextBox.Text.Trim() + "', field15 = '" + beDueTextBox.Text.Trim() + "', field16 = '" + enddueTextBox.Text.Trim() + "', field17 = '" + beuserTextBox.Text.Trim() + "', field18 = '" + enduserTextBox.Text.Trim() + "', field19 = '" + besmanTextBox.Text.Trim() + "', field20 = '" + endsmanTextBox.Text.Trim() + "', rd_field1 = '" + HiddenField1.Value + "', rd_field2 = '" + HiddenField2.Value + "', rd_field3 = '" + HiddenField3.Value + "', rd_field4 = '" + HiddenField4.Value + "', rd_field5 = '" + HiddenField5.Value + "', rd_field6 = '" + HiddenField6.Value + "', chk_field1 = '" + HiddenField8.Value + "', chk_field2 = '" + HiddenField9.Value + "', chk_field3 = '" + HiddenField10.Value + "', chk_field4 = '" + HiddenField11.Value + "', chk_field5 = '" + HiddenField12.Value + "', chk_field6 = '" + HiddenField13.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='open_order_report.aspx' ", conn);
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
            Label path = (Label)FormView1.FindControl("vFileLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='open_order_report.aspx'</script>");
            }
        }
        catch
        {

        }

    }
    protected void TextBox1_textchanged(object sender, EventArgs e)
    {
        //if (TextBox1.Text != "")
        //    Session["reorder_begin_cust"] = TextBox1.Text;
        //else
        //    Session["reorder_begin_cust"] = null;
    }

   
}
